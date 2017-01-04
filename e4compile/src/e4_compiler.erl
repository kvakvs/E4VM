-module(e4_compiler).
-export([process/1]).

-include("e4_forth.hrl").

%% @doc Takes filename as input, produces compiled BEAM AST and processes it
process(F) ->
    case compile:file(F, [to_kernel, binary, report]) of
        {ok, M, Kernel} ->
            IR1 = e4:try_do("Pass1 - Kernel Erlang to IC",
                            fun() -> e4_pass_kern:process(Kernel) end),
            IR2 = e4:try_do("Pass2 - Mark variable scopes",
                            fun() -> e4_pass_scopes:process(IR1) end),
            FlatForth  = e4:try_do("Pass3 - Convert IC to Forth",
                                   fun() -> e4_pass_flatten:process(IR2) end),
            FlatForth2 = e4:try_do("Pass4 - Optimize",
                                   fun() -> e4_pass_opt1:process(FlatForth)
                                   end),
            J1Prog = e4:try_do("Pass5 - Compile to J1 opcodes",
                               fun() -> e4_j1c:compile(M, FlatForth2) end),
            e4:try_do("Save binary output",
                fun() ->
                    IOList = e4_file:to_iolist(J1Prog),
                    file:write_file(e4_file:bin_filename(F),
                            iolist_to_binary(IOList))
                end);
        E ->
            io:format("~n~s: ~p~n", [F, E])
    end.
