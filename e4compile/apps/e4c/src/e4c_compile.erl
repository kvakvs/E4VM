-module(e4c_compile).
-export([process/1]).

-include_lib("e4c/include/forth.hrl").

%% @doc Takes filename as input, produces compiled BEAM AST and processes it
%% Returns module name and list of binary Forth tokens.
-spec process(string()) -> {module(), [binary()]}.
process(F) ->
    case compile:file(F, [to_kernel, binary, report]) of
        {ok, ModuleName, Kernel} ->
            IR1 = e4c:try_do("e4_pass_kern - Kernel Erlang to IC",
                             fun() -> e4_pass_kern:process(Kernel) end),
            IR2 = e4c:try_do("e4_pass_scopes - Mark variable scopes",
                             fun() -> e4_pass_scopes:process(IR1) end),
            FlatForth = e4c:try_do("e4_pass_flatten - Convert IC to Forth",
                                   fun() -> e4_pass_flatten:process(IR2) end),
            Forth = e4c:try_do("e4_pass_opt1 - Optimize",
                               fun() -> e4_pass_opt1:process(FlatForth) end),
            {ModuleName, Forth};
        Error ->
            io:format("~n~s: ~p~n", [F, Error])
    end.
