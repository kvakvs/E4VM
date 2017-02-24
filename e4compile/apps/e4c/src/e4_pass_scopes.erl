%%% @doc Process variable names and calculate locals for each scope

-module(e4_pass_scopes).

%% API
-export([process/1]).

-include_lib("e4c/include/forth.hrl").
-include_lib("e4c/include/e4c.hrl").

-import(e4_f1, [emit/2]).

process(IC = #cpp_block{}) ->
    Out = transform(IC),
    e4c:debug_write_term("e4c_pass_scopes.txt", Out),

%%    io:format("~s~n~s~n",
%%              [color:on_white(color:black(" PASS 2 Scopes ")),
%%               e4_print_ic:format_ic(Out, 0)]),
    Out.

transform(#cpp_block{before=Before, code=Code, aftr=After}) ->
    AllCode0 = [transform(Before), transform(Code), transform(After)],
    AllCode = lists:flatten(AllCode0),
    Out0 = #f2_block{code = AllCode},
    variables_pass(Out0, AllCode);
transform([H|T]) -> [transform(H) | transform(T)];
transform([]) -> [];
transform(Other) -> Other.

%% @doc Takes an f2 block with variable scope and follows the code counting
%% variable usages and args
variables_pass(Out0 = #f2_block{}, []) -> Out0;
variables_pass(Out0 = #f2_block{}, [H | Tail]) ->
    Out1 = process_one(Out0, H),
    variables_pass(Out1, Tail).

process_one(Out0, L) when is_list(L) ->
    lists:foldl(fun(Item, Blk) -> process_one(Blk, Item) end,
                Out0, L);
process_one(Out0 = #f2_block{}, #f_decl_arg{var=Arg}) ->
    Out1 = declare_arg(Out0, Arg),
%%    io:format("DECLARG scope ~s~n",
%%              [e4_f2:format_vars(Out1#f2_block.alloc_vars)]),
    Out1;
process_one(Out0 = #f2_block{}, #f_st{var=Arg}) ->
    Out1 = allocate_var(Out0, Arg),
%%    io:format("ST scope ~s~n", [e4_f2:format_vars(Out1#f2_block.alloc_vars)]),
    Out1;
process_one(Out0 = #f2_block{}, _Other) -> % ignore other and nested
    Out0.


allocate_var(Block0 = #f2_block{alloc_vars=Vars}, V) ->
    Block0#f2_block{alloc_vars=e4_f2:alloc_var(Vars, V, stack_frame)}.

declare_arg(Block0 = #f2_block{alloc_vars=Vars}, V) ->
    Block0#f2_block{alloc_vars=e4_f2:alloc_var(Vars, V, pre_existing)}.

%%add_alias(Block0 = #f2_block{alloc_vars=Vars}, NewV, ExistingV) ->
%%    Block0#f2_block{alloc_vars=e4_f2:add_alias(Vars, NewV, ExistingV)}.
