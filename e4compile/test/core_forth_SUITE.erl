-module(core_forth_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("../src/e4.hrl").

%% Test server callbacks
-export([suite/0, all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-compile([export_all]).

suite() ->
    [{timetrap,{minutes,1}}].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_testcase(_Case, Config) -> Config.
end_per_testcase(_Case, _Config) -> ok.

all() -> [
    compile_clause
].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

compile_clause() ->
    %% Try compile:
    %%      myfun(_, nil) -> 123;
    %%      ...
    RootScope = [#cf_var{name=cor0}, #cf_var{name=cor1}],
    Code = {c_case,
            [4, {}],
            {c_values,
             [4, {}],
             [{c_var, [4, {}], cor1},
              {c_var, [4, {}], cor0}]},
            [{c_clause,
              [4, {}],
              [{c_var, [], cor7}, {c_literal, [4, {}], nil}],
              {c_literal, [], true},
              {c_literal, [4, {}], 123}}
            ]},
    io:format(standard_error,
              color:yellowb("Compile: myfun(_, nil) -> 123") ++ "~n", []),
    Compiled = compile_helper(RootScope, Code),
    io:format(standard_error, "~s~n",
              [e4_c2cf:format_code(lists:flatten(Compiled), [])]).

compile_helper(RootScope, Code) ->
    Scope = #e4scope{vars=RootScope},
    S0 = e4_c2cf:module_new(),
    S1 = lists:foldl(fun(Var, St) -> e4_c2cf:stack_push(St, Var) end,
                     S0, RootScope),
    S2 = e4_c2cf:scope_push(S1, Scope),
    S = e4_c2cf:process_code(S2, Code),
    lists:reverse(e4_c2cf:get_code(S)).
