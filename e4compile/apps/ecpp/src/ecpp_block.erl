%%% @doc Code block operations
%%% @end

-module(ecpp_block).

%% API
-export([emit/2, scope/1]).

-include_lib("e4c/include/e4c.hrl").
-include_lib("ecpp/include/ecpp_ast.hrl").

%% @doc Add code to the end of the #f_block{}
emit(Root, AddCode) when not is_list(AddCode) -> emit(Root, [AddCode]);
emit(Root, AddCode) ->
    lists:foldl(fun emit_x_into_y/2, Root, AddCode).

emit_x_into_y(#cpp_var{}, _Blk) ->
    ?COMPILE_ERROR("should not emit variable");
emit_x_into_y(Nested, Root) when is_list(Nested) ->
    emit(Root, Nested);
emit_x_into_y(Op, Root = #cpp_block{code = Code}) ->
    Root#cpp_block{code = Code ++ [Op]};
emit_x_into_y(Op, Root = #cpp_if{true = Code}) ->
    Root#cpp_if{true = lists:flatten([Code]) ++ [Op]}.

scope(#cpp_block{scope = Scope}) -> Scope;
scope(#cpp_if{scope = Scope}) -> Scope.
