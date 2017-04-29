%%% @doc Binary module file writer.

-module(e4asm_file).

%% API
-export([to_iolist/1, bin_filename/1]).

to_iolist(Prog = #{'$' := e4mod}) ->
%%  Compr = uncompressed, % value: uncompressed | gzipped
  Content = [
    %% section("LABL", Compr, encode_labels(Compr, Prog)), % goes before code
    section("Co", encode_code(Prog)),
    section("Lt", encode_literals(Prog)),
    section("At", encode_atoms(Prog)),
    section("Im", encode_imports(Prog)),
    section("Xp", encode_exports(Prog)),
    section("Jt", encode_jumptabs(Prog)),
    section("Fn", encode_lambdas(Prog)),
    []
  ], [
    "E4",
    <<(iolist_size(Content)):32/big>>,
    Content
  ].


section(Tag, Data) ->
  [Tag, e4c:varint(byte_size(Data)), Data].


%% @doc Replace file ext in the filename with e4b
bin_filename(F) ->
  RDot = string:rchr(F, $.),
  string:sub_string(F, 1, RDot) ++ "e4b".


%% @doc Convert code from each function in the mod object to a single block of
%% code in the module file
encode_code(#{'$' := e4mod, funs := Funs}) ->
  Bin = [maps:get(code, F) || {_FunArity, F} <- Funs],
  erlang:iolist_to_binary(Bin).


%% @doc Convert atoms from mod object to atom section in the module file
encode_atoms(#{'$' := e4mod, atoms := Atoms}) ->
  Sorted = lists:keysort(2, Atoms), % assume orddict is a list of tuples
  Bin = [encode_atoms_one_atom(A) || {A, _} <- Sorted],
  erlang:iolist_to_binary([e4c:varint(length(Sorted)), Bin]).


encode_atoms_one_atom(A) when is_atom(A) ->
  StrA = atom_to_binary(A, utf8),
  [<<(byte_size(StrA)):8>>, StrA].


%% @doc Convert atoms from mod object to atom section in the module file
encode_exports(#{'$' := e4mod, exports := Exports, atoms := Atoms}) ->
  Sorted = lists:keysort(2, Exports), % assume orddict is a list of tuples
  Bin = [encode_exports_one_export(F, Arity, Atoms)
         || {{F, Arity}, _} <- Sorted],
  erlang:iolist_to_binary([e4c:varint(length(Sorted)), Bin]).


encode_exports_one_export(F, Arity, Atoms) ->
  FIndex = orddict:fetch(F, Atoms),
  <<(e4c:varint(FIndex))/binary,
    (e4c:varint(Arity))/binary>>.


encode_imports(#{'$' := e4mod, imports := Imports, atoms := Atoms}) ->
  Sorted = lists:keysort(2, Imports), % assume orddict is a list of tuples
  Bin = [encode_exports_one_import(M, F, Arity, Atoms)
         || {{M, F, Arity}, _} <- Sorted],
  erlang:iolist_to_binary([e4c:varint(length(Sorted)), Bin]).


encode_exports_one_import(M, F, Arity, Atoms) ->
  MIndex = orddict:fetch(M, Atoms),
  FIndex = orddict:fetch(F, Atoms),
  <<(e4c:varint(MIndex))/binary,
    (e4c:varint(FIndex))/binary,
    (e4c:varint(Arity))/binary>>.


encode_literals(#{'$' := e4mod, literals := Lit}) ->
  Sorted = lists:keysort(2, Lit), % assume orddict is a list of tuples
  Bin = [encode_literals_one_literal(L) || {L, _} <- Sorted],
  erlang:iolist_to_binary([e4c:varint(length(Sorted)), Bin]).


encode_literals_one_literal(L) ->
  erlang:term_to_binary(L).


encode_jumptabs(Mod = #{'$' := e4mod, jumptabs := JTabs}) ->
  Sorted = lists:keysort(2, JTabs), % assume orddict is a list of tuples
  Bin = [encode_jumptabs_one_jtab(J, Mod) || {J, _} <- Sorted],
  erlang:iolist_to_binary([e4c:varint(length(Sorted)), Bin]).


encode_jumptabs_one_jtab(J, Mod) ->
  [e4c:varint(length(J/2)),
   [e4asm_cte:encode(X, Mod) || X <- J]
  ].


encode_lambdas(Mod = #{'$' := e4mod, lambdas := Lambdas}) ->
  Sorted = lists:keysort(2, Lambdas), % assume orddict is a list of tuples
  Bin = [encode_lambdas_one_lambda(L, Mod) || {L, _} <- Sorted],
  erlang:iolist_to_binary([e4c:varint(length(Sorted)), Bin]).


encode_lambdas_one_lambda({{f, F}, NumFree}, Mod) ->
  [e4c:varint(F),
   e4c:varint(NumFree)].
