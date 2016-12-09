-module(core_forth_SUITE).

-include_lib("common_test/include/ct.hrl").

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

all() -> [].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

string(Config) ->
    S0.
