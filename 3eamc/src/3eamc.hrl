-define(SIGNATURE, <<"3EAM">>).
-define(SIG_EXPORTS, <<"ExpT">>).
-define(SIG_ATOMS, <<"Atom">>).
-define(SIG_CODE, <<"Code">>).

-define(MARK_FUNCTION, <<255:8>>).
-define(MARK_END_CODE, <<254:8>>).

-define(VAL_X,          1).
-define(VAL_Y,          2).
-define(VAL_ATOM,       3).
-define(VAL_MFARITY,    4).
-define(VAL_INTEGER,    5).
-define(VAL_LIT,        6).
-define(VAL_Z,          7). % internal Z register, 4 upper bits are register id

-define(OPCODE_MOVE,        1).
-define(OPCODE_CALL,        2).
-define(OPCODE_TAIL_CALL,   2+128).
-define(OPCODE_CONS,        3).
-define(OPCODE_RET,         4).

-define(OPCODE_SYSCALL,     6). % call id stored in upper 4 bits
    -define(SYSCALL_ALLOC,          0).
    -define(SYSCALL_DEALLOC,        1).
    -define(SYSCALL_RESOLVE_EXPORT, 2).
    -define(SYSCALL_TEST_HEAP,      3).

