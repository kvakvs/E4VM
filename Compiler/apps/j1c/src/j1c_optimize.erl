%%% @doc Pre-binary pass Forth optimizations
%%% @end

-module(j1c_optimize).

%% API
-export([optimize/2]).

-include_lib("j1c/include/j1.hrl").

optimize([], Acc) -> lists:flatten(lists:reverse(Acc));

optimize([#j1st{index = ST},
          #j1ld{index = LD} | Tail], Acc) when LD == ST ->
    %% Optimize: eliminate st N + ld N pairs
    optimize(Tail, Acc);

optimize([#j1st{index = ST},
          #j1ld{index = LD1},
          #j1ld{index = LD2} | Tail], Acc) when LD2 == ST ->
    %% Optimize: replace
    %% ST N; LD M; LD N
    %% with LD M; SWAP
    optimize(Tail, [[#j1ld{index = LD1}, <<"SWAP">>] | Acc]);

optimize([#j1st{index = ST},
          #j1lit{} = Lit,
          #j1ld{index = LD} | Tail], Acc) when LD == ST ->
    %% Optimize: replace
    %% ST N; LIT; LD N
    %% with LIT; SWAP
    optimize(Tail, [[Lit, <<"SWAP">>] | Acc]);

optimize([#j1st{index = ST},
          #j1atom{} = Atom,
          #j1ld{index = LD} | Tail], Acc) when LD == ST ->
    %% Optimize: replace
    %% ST N; ATOM; LD N
    %% with ATOM; SWAP
    optimize(Tail, [[Atom, <<"SWAP">>] | Acc]);

optimize([H | Tail], Acc) -> optimize(Tail, [H | Acc]).