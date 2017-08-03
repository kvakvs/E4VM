-module(test1).
-compile([export_all]).

test1() ->
    map_1(0, {1, 2, 3, test_return_var(4)}).

map_1(_, undefined) -> undefined;
map_1(F, {K, V, Smaller, Larger}) ->
    {K, F(K, V), map_1(F, Smaller), map_1(F, Larger)}.

test_return_var(X) ->
    Y = X * 2,
    Y.
