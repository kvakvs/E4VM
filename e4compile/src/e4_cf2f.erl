%%%-------------------------------------------------------------------
%%% @doc Compile Core Forth AST into Forth program consisting of words
%%% and literals. Optimize. Resolve variables.
%%% @end
%%%-------------------------------------------------------------------
-module(e4_cf2f).

%% API
-export([process/1]).

-include("e4_cf.hrl").
-include("e4_f.hrl").

-import(e4, [compile_error/2]).

process(CoreForth) ->
    F = process(CoreForth, []),
    io:format("~p~n", [F]),
    F.

process([], Code) -> lists:reverse(Code);
process([CF | Tail], Code) when is_list(CF) ->
    process(Tail, [process(CF) | Code]);
process([CF | Tail], Code) ->
    process(Tail, [process_op(CF) | Code]);
process(Op, []) -> % if a single item is given, like a root block
    process_op(Op).

process_op(A) when is_atom(A) -> A; % pass through forth words
process_op(#cf_block{before=Before, code=Code, 'after'=After}) ->
    process([Before, Code, After], []);
process_op(#cf_comment{comment=C}) ->
    ['(', C, ')'];
process_op(#cf_mfarity{mod=M, fn=F, arity=A}) ->
    ['\'MFA', M, F, A];
process_op(#cf_retrieve{var=V}) ->
    [V, 'LD'];
process_op(#cf_store{var=V}) ->
    [V, 'ST'];
process_op(#cf_lit{val=V}) ->
    ['\'LIT', V];
process_op(#cf_alias{var=Var, alt=A}) ->
    ['(', 'ALIAS', Var, A, ')'];
process_op(CF) ->
    compile_error("E4Core4: Unknown op ~p~n", [CF]).
