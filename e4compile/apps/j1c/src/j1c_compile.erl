-module(j1c_compile).
-export([process/3]).

%%-include_lib("e4c/include/forth.hrl").

%% @doc Takes: parsed words (list of binary) as input
%% Writes binary e4b file with Forth bytecode.
-spec process(string(), module(), [binary()]) -> ok.
process(InputPath, ModuleName, Input) ->
    #{p := J1Prog, result := J1Preprocessed} =
        e4c:try_do("j1c_pp - Preprocess Forth",
                   fun() -> j1c_pp:forth_preproc(ModuleName, Input) end),
    J1Forth = e4c:try_do(
        "j1c_pass_forth - Compile to J1 opcodes",
        fun() -> j1c_pass_forth:compile(J1Prog, J1Preprocessed) end),
    J1Bin1 = e4c:try_do("j1c_pass_link - J1 link label addresses",
                        fun() -> j1c_pass_link:link_pass(J1Forth) end),
%%    J1Bin2 = e4c:try_do("j1c_pass_bin - Compile J1 IC to binary",
%%                        fun() -> j1c_pass_bin:compile(J1Bin1) end),
    e4c:try_do("Save binary output",
               fun() ->
                   IOList = j1c_file:to_iolist(J1Bin1),
                   file:write_file(j1c_file:bin_filename(InputPath),
                                   iolist_to_binary(IOList))
               end),
    ok.
