%%% @doc Binary module file writer.
%%% Can handle text (for debugging) or binary output format.
%%% Will compress parts of the output.
%%% @end

-module(uasm_file).

%% API
-export([
  make_filename/2,
  to_iolist/2
]).

%% @doc Given a module object creates its final binary representation
%% which is then written by the caller (uasm_compile module).
-spec to_iolist(Format :: text | binary, Program :: map()) -> iolist().
to_iolist(Format, Prog = #{'$' := module}) ->
  Content0 = [
    % atoms must go before: imp/exports, jtabs, code
    section(Format, atoms, encode_atoms(Format, Prog)),
    section(Format, code, encode_code(Format, Prog)),
    section(Format, literals, encode_literals(Format, Prog)),
    section(Format, imports, encode_imports(Prog)),
    section(Format, exports, encode_exports(Prog)),
    section(Format, jumptabs, encode_jumptabs(Prog)),
    section(Format, lambdas, encode_lambdas(Prog)),
    []
  ],
  Content1 = io_lib:format("~p", [Content0]),
  [module_header(Format, erlang:iolist_size(Content1)),
   Content1].


module_header(text, ContentSize) ->
  [];
module_header(binary, ContentSize) ->
  version("E432", "E464", get(e4_machine_word_bits)),
  big32(ContentSize).


section(binary, Tag, Data) ->
  [Tag,
   big32(byte_size(Data)),
   Data]; % TODO: 4 alignment for ARM

section(text, Tag, Data) ->
  #{'$section' => Tag,
    content => Data}.


%% @doc Replace file ext in the filename with e4b
make_filename(F, Extension) ->
  RDot = string:rchr(F, $.),
  string:sub_string(F, 1, RDot) ++ Extension.


%% @doc Convert code from each function in the mod object to something writable
%% to the final output file
-spec encode_code(text | binary, #{'$' => module}) -> iolist().
encode_code(_Format, #{'$' := module} = Mod) ->
  %% Collect code output for every function in the module
  %% TODO: Store functions separately + optimize unused?
  #{'$' := huffman,
    tree := Tree,
    output := Output} = uasm_huffman:encode_funs(Mod),
  %% TODO: Tree can be inferred once and hardcoded in the emulator

  %% Take final code from each function and store them together with function
  %% name, and arity, and size into a nice solid binary chunk
  FuncChunks = lists:map(
    fun(#{'$' := one_fun,
          name := {FName, FArity},
          labels := FLabels,
          output := FBin}) ->
      #{'$function' => {FName, FArity},
        labels => uasm_huffman:encode_labels(FLabels),
        labels_debug => maps:from_list(FLabels),
        code_size => byte_size(FBin),
        code => FBin}
    end,
    Output),
  EncodedTree0 = uasm_huffman:encode_tree(Tree, 6), % 6 = 2^round(log2(maxOp))
  EncodedTree = uasm_huffman:merge_bits_into_binary(EncodedTree0),
  #{tree => EncodedTree,
    functions => FuncChunks}.


%% @doc Convert atoms from mod object to atom section in the module file
encode_atoms(binary, Mod) ->
  #{count := AtomCount,
    atoms := Encoded} = encode_atoms(text, Mod),
  erlang:iolist_to_binary([uerlc:varint(AtomCount), Encoded]);

encode_atoms(text, #{'$' := module, atoms := Atoms}) ->
  Sorted = lists:keysort(2, Atoms), % assume orddict is a list of tuples
  EncodedList = [encode_atoms_one_atom(A) || {A, _Index} <- Sorted],
  EncodedFlat = binary_to_list(iolist_to_binary(EncodedList)),
  Compressed = uasm_huffman:encode(EncodedFlat),
  #{atoms => EncodedList,
    count => length(Sorted),
    compressed => Compressed}.


-ifdef(COMPRESS_ENABLE).
%% @doc Runs zlib compress on the data and writes out section with size headers
%% encoded as 7-bit multibyte varints
zlib_compress(Name, OriginalData) ->
  CompressedData = zlib:compress(OriginalData),
  io:format("Compressed ~s: was ~B bytes, now ~B~n",
            [Name, byte_size(OriginalData), byte_size(CompressedData)]),
  <<(uerlc:varint(OriginalData))/binary,
    (uerlc:varint(CompressedData)):32/big,
    CompressedData/binary>>.
-endif.


encode_atoms_one_atom(A) when is_atom(A) ->
  StrA = atom_to_binary(A, utf8),
  [<<(byte_size(StrA)):8>>, StrA].


%% @d oc Convert labels to label section in the module file
%%encode_labels(#{'$' := module, labels := Labels}) ->
%%  io:format("labels ~p~n", [Labels]),
%%  Sorted = lists:keysort(1, Labels), % assume orddict is a list of tuples
%%  Bin = [encode_labels_one_label(L) || {_, L} <- Sorted],
%%  erlang:iolist_to_binary([big32(length(Sorted)), Bin]).


%%encode_labels_one_label(L) ->
%%  [e4c:varint(L)].


%% @doc Convert atoms from mod object to atom section in the module file
encode_exports(#{'$' := module, exports := Exports, atoms := Atoms}) ->
  Sorted = lists:keysort(1, Exports), % assume orddict is a list of tuples
  io:format("~p~n", [Sorted]),
  Bin = [encode_exports_one_export(FunArity, Label, Atoms)
         || {FunArity, Label} <- Sorted],
  erlang:iolist_to_binary([big32(length(Sorted)), Bin]).


encode_exports_one_export({Fun, Arity}, Label, Atoms) ->
  FIndex = orddict:fetch(Fun, Atoms),
  <<(uerlc:varint(FIndex))/binary,
    (uerlc:varint(Arity))/binary,
    (uerlc:varint(Label))/binary>>.


encode_imports(#{'$' := module, imports := Imports, atoms := Atoms}) ->
  Sorted = lists:keysort(2, Imports), % assume orddict is a list of tuples
  Bin = [encode_exports_one_import(M, F, Arity, Atoms)
         || {{M, F, Arity}, _} <- Sorted],
  erlang:iolist_to_binary([big32(length(Sorted)), Bin]).


encode_exports_one_import(M, F, Arity, Atoms) ->
  MIndex = orddict:fetch(M, Atoms),
  FIndex = orddict:fetch(F, Atoms),
  <<(uerlc:varint(MIndex))/binary,
    (uerlc:varint(FIndex))/binary,
    (uerlc:varint(Arity))/binary>>.


encode_literals(binary, Mod) ->
  #{count := Count,
    literals := Literals} = encode_literals(text, Mod),
  erlang:iolist_to_binary([uerlc:varint(Count), Literals]);

encode_literals(text, #{'$' := module, literals := Lit}) ->
  Sorted = lists:keysort(2, Lit), % assume orddict is a list of tuples
  EncodedDebug = [{L, encode_literals_one_literal(L)} || {L, _} <- Sorted],
  Encoded = [encode_literals_one_literal(L) || {L, _} <- Sorted],
  #{debug => maps:from_list(EncodedDebug),
    literals => Encoded,
    count => length(Sorted)}.


encode_literals_one_literal(L) ->
  erlang:term_to_binary(L).


encode_jumptabs(Mod = #{'$' := module, jumptabs := JTabs}) ->
  Sorted = lists:keysort(2, JTabs), % assume orddict is a list of tuples
  Bin = [encode_jumptabs_one_jtab(J, Mod) || {J, _} <- Sorted],
  erlang:iolist_to_binary([big32(length(Sorted)), Bin]).


encode_jumptabs_one_jtab(J, Mod) ->
  [uerlc:varint(length(J / 2)),
   [uasm_encode_cte:encode(X, Mod) || X <- J]
  ].


encode_lambdas(Mod = #{'$' := module, lambdas := Lambdas}) ->
  Sorted = lists:keysort(2, Lambdas), % assume orddict is a list of tuples
  Bin = [encode_lambdas_one_lambda(L, Mod) || {L, _} <- Sorted],
  erlang:iolist_to_binary([big32(length(Sorted)), Bin]).


encode_lambdas_one_lambda({{f, Label}, NumFree}, _Mod) ->
  [uerlc:varint(Label), % this should somehow resolve to atom function name maybe
   uerlc:varint(NumFree)].


version(V32, _V64, 32) -> V32;
version(_V32, V64, 64) -> V64.


big32(X) ->
  <<X:32/big>>.
