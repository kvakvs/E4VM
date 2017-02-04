%%% @doc Link label addresses and decide if some addresses can be written
%%% in a more compact form
%%% @end

-module(j1c_pass_link).

%% API
-export([link/1]).

-include_lib("j1c/include/j1.hrl").
-include_lib("j1c/include/j1binary.hrl").

link(#j1bin_prog{output = Input} = Prog) ->
    link(Prog, Input, []).

%% @doc Final phase: find commands which used label references and replace
%% offset in them with label address. This may require shifting input right
%% with an extra word for longer address, and also shifting all labels above
%% this address.
link(Prog = #j1bin_prog{}, [], Acc) ->
    New = lists:flatten(lists:reverse(Acc)),
    Prog#j1bin_prog{output = New};

link(Prog = #j1bin_prog{}, [<<?J1INSTR_CALL:?J1INSTR_WIDTH,
                              Label:?J1OP_INDEX_WIDTH>> | Tail], Acc) ->
    link(Prog, Tail, [Label | Acc]);

link(Prog = #j1bin_prog{}, [H | Tail], Acc) ->
    link(Prog, Tail, [H | Acc]).
