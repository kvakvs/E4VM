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
-export([link_pass/1]).

-include_lib("e4c/include/e4c.hrl").
-include_lib("j1c/include/j1.hrl").
-include_lib("j1c/include/j1bytecode.hrl").

link_pass(#j1prog{output = Input} = Prog0) ->
%%    e4c:debug_write_term("j1c_pass_link-0.txt", Input1),

    ?ASSERT(Prog0#j1prog.pc =:= 0, "PC must be zero"),
    #{p := Prog1, bin := Input2}
        = j1c_compile_bin:compile_segment(Prog0, Input),

%%    Input1 = partition(Input, []),
%%    %% For each item in partitioned input list, apply compile_segment to it
%%    %% so that we get a list of #j1compiled{} and #j1jump{}
%%    #{p := Prog1, accum := Input2} = lists:foldl(
%%        fun(Code, #{p := Prog, accum := Acc}) ->
%%            #{p := ProgA, bin := Bin} =
%%                j1c_compile_bin:compile_segment(Prog, Code),
%%            #{p => ProgA, accum => [Bin | Acc]}
%%        end,
%%        #{p => Prog0, accum => []},
%%        Input1),
    e4c:debug_write_term("j1c_pass_link-1.txt", Input2),

    Result1 = link(Prog1#j1prog{output = []}, Input2, []),
    e4c:debug_write_term("j1c_pass_link-2.txt", Result1),

    Result2 = lists:flatten(Result1),
    e4c:debug_write_term("j1c_pass_link-3.txt", Result2),

    Prog1#j1prog{output = Result2}.

%% @doc Final phase: find commands which used label references and replace
%% offset in them with label address. This may require shifting input right
%% with an extra word for longer address, and also shifting all labels above
%% this address.
link(#j1prog{}, [], Acc) ->
    lists:flatten(lists:reverse(Acc));

link(Prog = #j1prog{},
     [[LitVarint, <<?J1BYTE_INSTR_JUMP:8>>] | Tail], Acc) ->
    link(Prog, Tail, [#{jump => LitVarint} | Acc]);

link(Prog = #j1prog{},
     [[LitVarint, <<?J1BYTE_INSTR_JUMP_COND:8>>] | Tail], Acc) ->
    link(Prog, Tail, [#{jump => LitVarint, type => z} | Acc]);

link(Prog = #j1prog{}, [H | Tail], Acc) ->
    link(Prog, Tail, [H | Acc]).

%% @doc Given flat list of #j1*{} forth commands
%% Returns: partitioned list using jump commands as separators
%% 1> L = [1,2,3,{j1jump, x, y}, 4, 5]. ---> [1,2,3,{j1jump,x,y},4,5]
%% 2> j1c_pass_link:partition(L, []). ---> [[1,2,3],{j1jump,x,y},[4,5]]
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
