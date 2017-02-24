-module(e4c_compile).
-export([process/1]).

-include_lib("e4c/include/forth.hrl").
-include_lib("compiler/src/core_parse.hrl").

%% @doc Takes filename as input, produces compiled BEAM AST and processes it
%% Returns module name and list of binary Forth tokens.
-spec process(string()) -> {module(), [binary()]}.
process(F) ->
    Opt = [report_errors, report_warnings, {outdir, "priv"}],
%%    e4c:debug_write_term("e4c_c_core.txt", c:c(F, Opt ++ [to_core])),
%%    e4c:debug_write_term("e4c_c_core_b.txt", c:c(F, Opt ++ [to_core, binary])),
    e4c:debug_write_term("e4c_c_kern.txt", c:c(F, Opt ++ [to_kernel])),

    {ok, _, Kern0} = compile:file(F, Opt ++ [to_kernel, binary]),
    Kern = record_to_map(Kern0),
    e4c:debug_write_term("e4c_c_kern_b.txt", Kern),

    case compile:file(F, [to_kernel, binary, report]) of
        {ok, ModuleName, Kernel} ->
            IR1 = e4c:try_do("e4_pass_kern - Kernel Erlang to IC",
                             fun() -> e4_pass_kern:process(Kernel) end),
%%            IR2 = e4c:try_do("e4_pass_scopes - Mark variable scopes",
%%                             fun() -> e4_pass_scopes:process(IR1) end),
%%            FlatForth = e4c:try_do("e4_pass_flatten - Convert IC to Forth",
%%                                   fun() -> e4_pass_flatten:process(IR2) end),
%%            Forth = e4c:try_do("e4_pass_opt1 - Optimize",
%%                               fun() -> e4_pass_opt1:process(FlatForth) end),
            ecpp_render:write(F, IR1),
            {ModuleName, IR1};
        Error ->
            io:format("~n~s: ~p~n", [F, Error])
    end.

%% Convert Kernel Erlang AST Tree into maps with all fields named for pleasant
%% reading (can open file as Erlang file later and use source formatting and
%% syntax and stuff)
-define(R2M(Record),
    record_to_map(#Record{} = Rec) ->
        L = lists:zip(record_info(fields, Record), tl(tuple_to_list(Rec))),
        L1 = proplists:delete(anno, L),
        L2 = [{K, record_to_map(V)} || {K, V} <- L1],
        L3 = [{'!', Record} | L2],
        maps:from_list(L3)
    ).

record_to_map(L) when is_list(L) -> lists:map(fun record_to_map/1, L);
%%?R2M(k);
?R2M(k_alt);
?R2M(k_atom);
?R2M(k_bif);
?R2M(k_enter);
?R2M(k_fdef);
?R2M(k_guard);
?R2M(k_guard_clause);
?R2M(k_int);
?R2M(k_match);
?R2M(k_mdef);
?R2M(k_nil);
?R2M(k_remote);
?R2M(k_return);
?R2M(k_select);
?R2M(k_seq);
?R2M(k_test);
?R2M(k_try);
?R2M(k_tuple);
?R2M(k_type_clause);
?R2M(k_val_clause);
?R2M(k_var);
record_to_map(X) -> X.
