-module('3eamc_asmbin').
-include("3eamc.hrl").

%% API
-export([bin/1]).
-import('3eamc_encode', [varint/1, val_int/1, val_zreg/1, val_xreg/1]).

%% Encode
bin({'Gluon_move', A, B}) ->
    [?OPCODE_MOVE, A, B];

bin({'Gluon_syscall', Id}) when Id >= 0 andalso Id =< 15 ->
    ?OPCODE_SYSCALL + (Id bsr 4);

bin({'Gluon_call', Arity}) ->
    [?OPCODE_CALL, varint(Arity)];

bin({'Gluon_tail_call', Arity}) ->
    [?OPCODE_TAIL_CALL, varint(Arity)].
