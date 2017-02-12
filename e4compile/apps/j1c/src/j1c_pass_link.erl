%%% @doc Link label addresses and decide if some addresses can be written
%%% in a more compact form.
%%% Input: flat list of #j1*{} records is passed in
%%% Algorithm:
%%% 1. List contents are partitioned using jump commands as separators
%%% 2. Each partition is compiled to binary, and their size is determined
%%% 3. Now that we know approximate label values, we give values to each
%%%     jump command from the Labels table
%%% 4. Revisit all jump commands to see if their value still fits into a short
%%%     address otherwise shift all labels after it accordingly. Revisit
%%%     affected jumps and edit them too
%%% @end

-module(j1c_pass_link).

%% API
-export([link/1]).

-include_lib("j1c/include/j1.hrl").
-include_lib("j1c/include/j1binary.hrl").

link(#j1prog{output = Input} = Prog) ->
    Input1 = partition(Input, []),
    e4c:debug_write_term("j1c_pass_link-1.txt", Input1),
    link(Prog, Input1, []).

%% @doc Final phase: find commands which used label references and replace
%% offset in them with label address. This may require shifting input right
%% with an extra word for longer address, and also shifting all labels above
%% this address.
link(Prog = #j1prog{}, [], Acc) ->
    New = lists:flatten(lists:reverse(Acc)),
    Prog#j1prog{output = New};

%%link(Prog = #j1bin_prog{}, [<<?J1INSTR_CALL:?J1INSTR_WIDTH,
%%                              Label:?J1OP_INDEX_WIDTH>> | Tail], Acc) ->
%%    link(Prog, Tail, [Label | Acc]);

link(Prog = #j1prog{}, [H | Tail], Acc) ->
    link(Prog, Tail, [H | Acc]).

%% @doc Given flat #j1*{} list partition it using jump commands as separators
%% 1> L = [1,2,3,{j1jump, x, y}, 4, 5].
%% [1,2,3,{j1jump,x,y},4,5]
%% 2> j1c_pass_link:partition(L, []).
%% [[1,2,3],{j1jump,x,y},[4,5]]
%%
partition([], Acc) -> lists:reverse(Acc);
partition(Input, Acc) ->
    case lists:splitwith(fun is_not_separator/1, Input) of
        {Head1, [Separator | Tail]} ->
            %% Separator goes first because we reverse in the end
            partition(Tail, [Separator, Head1 | Acc]);
        {Head2, []} ->
            partition([], [Head2 | Acc])
    end.

is_not_separator(#j1jump{}) -> false;
is_not_separator(_) -> true.
