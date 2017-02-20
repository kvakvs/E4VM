%%% @doc Binary module file writer.
-module(j1c_file).

%% API
-export([to_iolist/1, bin_filename/1]).

-include_lib("j1c/include/j1.hrl").
-include_lib("e4c/include/forth.hrl").

to_iolist(Prog = #j1prog{}) ->
    Compr = uncompressed, % value: uncompressed | gzipped
    Content = [
        section("LABL", Compr, encode_labels(Compr, Prog)), % goes before code
        section("CODE", Compr, encode_code(Compr, Prog)),
        section("LTRL", Compr, encode_literals(Compr, Prog)),
        section("ATOM", Compr, encode_atoms(Compr, Prog)),
        section("EXPT", Compr, encode_exports(Compr, Prog)),
        []
    ],
    [
        "E4J1", e4c:varint(iolist_size(Content)), Content
    ].

section(Tag, Compression, Data) ->
    [encode_tag(Compression, Tag), e4c:varint(byte_size(Data)), Data].

bin_filename(F) ->
    RDot = string:rchr(F, $.),
    string:sub_string(F, 1, RDot) ++ "e4b".

encode_tag(uncompressed, T) -> T.
%%encode_tag(gzipped, T) -> string:to_lower(T).

encode_block(uncompressed, D) -> D.
%%encode_block(gzipped, D) -> [e4c:varint(iolist_size(D)), zlib:gzip(D)].

encode_literals(Compr, #j1prog{literals=LitTab}) ->
    LitTab1 = lists:keysort(2, LitTab),
    LitTab2 = [
        e4c:varint(length(LitTab1)),
        [term_to_binary(Val) || {Val, _Index} <- LitTab1]
    ],
%%    io:format("~p~n", [LitTab1]),
    Enc1 = encode_block(Compr, LitTab2),
    iolist_to_binary(Enc1).

encode_atoms(Compr, #j1prog{atoms=AtomTab}) ->
    AtomTab1 = lists:keysort(2, AtomTab),
    EncodeAtom = fun(Str) ->
            <<(e4c:varint(byte_size(Str)))/binary, Str/binary>>
        end,
    AtomTab2 = [
        e4c:varint(length(AtomTab1)),
        [EncodeAtom(Val) || {Val, _Index} <- AtomTab1]
    ],
    Enc1 = encode_block(Compr, AtomTab2),
    iolist_to_binary(Enc1).

encode_code(Compr, #j1prog{output=Code}) ->
    Code2 = encode_block(Compr, Code),
    iolist_to_binary(Code2).

encode_export_fun(#j1prog{atoms=Atoms, dict=Dict}, Name, Arity)
    when is_binary(Name) ->
        AtomId = orddict:fetch(Name, Atoms),
        Offset = orddict:fetch({Name, Arity}, Dict),
%%        io:format("write export ~s/~p at ~p~n", [Name, Arity, Offset]),
        <<(e4c:varint(AtomId))/binary,
          (e4c:varint(Arity))/binary,
          (e4c:varint(Offset))/binary>>.

encode_exports(Compr, Prog = #j1prog{exports=Expt}) ->
    ExpTab1 = lists:keysort(2, Expt),
    ExpTab2 = [
        e4c:varint(length(ExpTab1)),
        [encode_export_fun(Prog, Fun, Arity) || {Fun, Arity} <- ExpTab1]
    ],
    Enc1 = encode_block(Compr, ExpTab2),
    iolist_to_binary(Enc1).

encode_labels(Compr, #j1prog{labels=Labels}) ->
    Labels1 = lists:keysort(1, Labels),
    Labels2 = [
        e4c:varint(length(Labels1)),
        [e4c:varint(Offset) || {_Lbl, Offset} <- Labels1]
    ],
    Enc1 = encode_block(Compr, Labels2),
    iolist_to_binary(Enc1).
