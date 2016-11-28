-module('3eamc_asm').
-include("3eamc.hrl").

%% API
-export([
    asm_apply/2,
    asm_call/3,
    asm_move/2,
    asm_syscall/1
]).
-import('3eamc_encode', [varint/1, val_int/1, val_zreg/1, val_xreg/1]).

%% A,B must be encoded value or encoded destination, not an integer
asm_move(A, B) when not is_integer(A), not is_integer(B) ->
    {'Gluon_move', A, B}.

asm_syscall(Id) when Id >= 0 andalso Id =< 15 ->
    {'Gluon_syscall', Id}.

asm_call(label = _DestKind, CallOpcode, {Label, Arity})
    when is_atom(CallOpcode) ->
    %% Just load label address (resolved at load-time) and then call
    [
        asm_move(Label, val_zreg(0)),
        {CallOpcode, val_int(Arity)}
    ];
asm_call(mfarity = _DestKind, CallOpcode, {Mod, Fun, Arity})
    when is_atom(CallOpcode) ->
    %% Inject a syscall to resolve MFArity and then call
    [
        asm_move(Mod,   {z, 0}),
        asm_move(Fun,   {z, 1}),
        asm_move(Arity, {z, 2}),
        asm_syscall(?SYSCALL_RESOLVE_EXPORT),
        {CallOpcode, Arity}
    ].

%% Args go in x[0]..x[Arity-1], module goes in x[Arity], fun in x[Arity+1]
asm_apply(ApplyOpcode, Arity) when is_atom(ApplyOpcode) ->
    [
        asm_move({x, Arity},     {z, 0}),
        asm_move({x, Arity + 1}, {z, 1}),
        asm_move(Arity,          {z, 2}),
        asm_syscall(?SYSCALL_RESOLVE_EXPORT),
        {ApplyOpcode, Arity}
    ].
