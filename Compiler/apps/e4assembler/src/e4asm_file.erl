%%% @doc Binary module file writer.

-module(e4asm_file).

%% API
-export([to_iolist/2, bin_filename/1]).

%% @doc Given a e4mod module object creates its final binary representation
%% which is then written by the caller (e4asm_compile module).
-spec to_iolist(Format :: text | binary, Program :: map()) -> iolist().
to_iolist(Format, Prog = #{'$' := e4mod}) ->
  Content = [
    %% section("LABL", Compr, encode_labels(Compr, Prog)), % goes before code
    section(Format, "Co", encode_code(Prog)),
    %section(Format, "Lb", encode_labels(Prog)), % must go before exports
    section(Format, "Lt", encode_literals(Prog)),
    % atoms must go before: imp/exports, jtabs
    section(Format, "At", encode_atoms(Prog)),
    section(Format, "Im", encode_imports(Prog)),
    section(Format, "Xp", encode_exports(Prog)),
    section(Format, "Jt", encode_jumptabs(Prog)),
    section(Format, "Fn", encode_lambdas(Prog)),
    []
  ],
  [
    module_header(Format, iolist_size(Content)),
    Content
  ].


module_header(binary, ContentSize) ->
  version("E432", "E464", get(e4_machine_word_bits)),
  big32(ContentSize).


section(Tag, Data) ->
  [Tag,
   big32(byte_size(Data)),
   Data]. % TODO: 4 alignment for ARM


%% @doc Replace file ext in the filename with e4b
bin_filename(F) ->
  RDot = string:rchr(F, $.),
  string:sub_string(F, 1, RDot) ++ "e4b".


%% @doc Convert code from each function in the mod object to a single block of
%% code in the module file
encode_code(#{'$' := e4mod, funs := Funs}) ->
  Bin = [maps:get(binary, F) || {_FunArity, F} <- Funs],
  erlang:iolist_to_binary(Bin).


%% @doc Convert atoms from mod object to atom section in the module file
encode_atoms(#{'$' := e4mod, atoms := Atoms}) ->
  Sorted = lists:keysort(2, Atoms), % assume orddict is a list of tuples
  Bin = [encode_atoms_one_atom(A) || {A, _} <- Sorted],
  erlang:iolist_to_binary([big32(length(Sorted)), Bin]).


encode_atoms_one_atom(A) when is_atom(A) ->
  StrA = atom_to_binary(A, utf8),
  [<<(byte_size(StrA)):8>>, StrA].


%% @d oc Convert labels to label section in the module file
%%encode_labels(#{'$' := e4mod, labels := Labels}) ->
%%  io:format("labels ~p~n", [Labels]),
%%  Sorted = lists:keysort(1, Labels), % assume orddict is a list of tuples
%%  Bin = [encode_labels_one_label(L) || {_, L} <- Sorted],
%%  erlang:iolist_to_binary([big32(length(Sorted)), Bin]).


%%encode_labels_one_label(L) ->
%%  [e4c:varint(L)].


%% @doc Convert atoms from mod object to atom section in the module file
encode_exports(#{'$' := e4mod, exports := Exports, atoms := Atoms}) ->
  Sorted = lists:keysort(1, Exports), % assume orddict is a list of tuples
  io:format("~p~n", [Sorted]),
  Bin = [encode_exports_one_export(FunArity, Label, Atoms)
         || {FunArity, Label} <- Sorted],
  erlang:iolist_to_binary([big32(length(Sorted)), Bin]).


encode_exports_one_export({Fun, Arity}, Label, Atoms) ->
  FIndex = orddict:fetch(Fun, Atoms),
  <<(e4c:varint(FIndex))/binary,
    (e4c:varint(Arity))/binary,
    (e4c:varint(Label))/binary>>.


encode_imports(#{'$' := e4mod, imports := Imports, atoms := Atoms}) ->
  Sorted = lists:keysort(2, Imports), % assume orddict is a list of tuples
  Bin = [encode_exports_one_import(M, F, Arity, Atoms)
         || {{M, F, Arity}, _} <- Sorted],
  erlang:iolist_to_binary([big32(length(Sorted)), Bin]).


encode_exports_one_import(M, F, Arity, Atoms) ->
  MIndex = orddict:fetch(M, Atoms),
  FIndex = orddict:fetch(F, Atoms),
  <<(e4c:varint(MIndex))/binary,
    (e4c:varint(FIndex))/binary,
    (e4c:varint(Arity))/binary>>.


encode_literals(#{'$' := e4mod, literals := Lit}) ->
  Sorted = lists:keysort(2, Lit), % assume orddict is a list of tuples
  Bin = [encode_literals_one_literal(L) || {L, _} <- Sorted],
  erlang:iolist_to_binary([big32(length(Sorted)), Bin]).


encode_literals_one_literal(L) ->
  erlang:term_to_binary(L).


encode_jumptabs(Mod = #{'$' := e4mod, jumptabs := JTabs}) ->
  Sorted = lists:keysort(2, JTabs), % assume orddict is a list of tuples
  Bin = [encode_jumptabs_one_jtab(J, Mod) || {J, _} <- Sorted],
  erlang:iolist_to_binary([big32(length(Sorted)), Bin]).


encode_jumptabs_one_jtab(J, Mod) ->
  [e4c:varint(length(J/2)),
   [e4asm_cte:encode(X, Mod) || X <- J]
  ].


encode_lambdas(Mod = #{'$' := e4mod, lambdas := Lambdas}) ->
  Sorted = lists:keysort(2, Lambdas), % assume orddict is a list of tuples
  Bin = [encode_lambdas_one_lambda(L, Mod) || {L, _} <- Sorted],
  erlang:iolist_to_binary([big32(length(Sorted)), Bin]).


encode_lambdas_one_lambda({{f, Label}, NumFree}, _Mod) ->
  [e4c:varint(Label), % this should somehow resolve to atom function name maybe
   e4c:varint(NumFree)].


version(V32, _V64, 32) -> V32;
version(_V32, V64, 64) -> V64.


big32(X) ->
  <<X:32/big>>.
