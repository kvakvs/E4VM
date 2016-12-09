-module(test1).
-compile([export_all]).

map_1(_, nil) -> nil;
map_1(F, {K, V, Smaller, Larger}) ->
    {K, F(K, V), map_1(F, Smaller), map_1(F, Larger)}.
