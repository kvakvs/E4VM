%%% @doc Value helpers

-module(e4_helper).

%% API
-export([is_value_known/2, is_in_the_scope/2]).

-include("e4_forth.hrl").
-include("e4.hrl").

%% @doc Consult with scope and find out if value contains only known parts
%% such as bound variables and literals, or some dynamic parts such as calls
%% and (free) unbound variables. Having a known or dynamic value allows
%% binding it to a temporary before some other calculation.
-spec is_value_known(f_block(), any()) -> boolean().
is_value_known(Block, Elements) when is_list(Elements) ->
    lists:all(fun(E) -> is_value_known(Block, E) end,
              Elements);
is_value_known(_Block, #k_literal{}) -> true;
is_value_known(_Block, #k_int{})     -> true;
is_value_known(_Block, #k_float{})   -> true;
is_value_known(_Block, #k_atom{})    -> true;
is_value_known(_Block, #k_nil{})     -> true;
is_value_known(Block = #f_block{}, #k_tuple{es=Elements}) ->
    is_value_known(Block, Elements);
is_value_known(Block = #f_block{}, #k_cons{hd=H, tl=T}) ->
    is_value_known(Block, H) andalso is_value_known(Block, T);
is_value_known(Block = #f_block{}, #f_block{code=Code}) ->
    is_value_known(Block, Code);
%%is_value_known(_Block, #k_var{})     -> true; % assuming var exists
is_value_known(#f_block{scope=Scope}, #k_var{} = Var) ->
    is_in_the_scope(Scope, Var).

%% @doc Given a scope from an #f_block{} and a #k_var{} checks if the var exists
%% in that scope (bound) or doesn't (free)
is_in_the_scope(Scope, #k_var{name=Name}) ->
    lists:member(Name, Scope).
