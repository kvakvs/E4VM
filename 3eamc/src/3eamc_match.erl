-module('3eamc_match').

%% API
-export([]).

-include_lib("compiler/src/core_parse.hrl").

%%process_code(State0, #c_clause{pats=Pats, guard=Guard, body=Body}) ->
%%    State = pattern_match(State0, Pats),
%%    case Guard of
%%        #c_literal{val = true} ->
%%            process_code(State, Body);
%%        _ ->
%%            State1 = process_code(State, Guard),
%%            State2 = f_emit(State1, 'IF'),
%%            State3 = process_code(State2, Body),
%%            f_emit(State3, 'THEN')
%%    end.
