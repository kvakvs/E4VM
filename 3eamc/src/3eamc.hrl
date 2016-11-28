-define(SIGNATURE, <<"3EAM">>).
-define(SIG_EXPORTS, <<"ExpT">>).
-define(SIG_ATOMS, <<"Atom">>).
-define(SIG_CODE, <<"Code">>).

-define(MARK_FUNCTION, <<255:8>>).
-define(MARK_END_CODE, <<254:8>>).

-define(VAL_NIL,        0).
-define(VAL_X,          1).
-define(VAL_Y,          2).
-define(VAL_ATOM,       3).
-define(VAL_MFARITY,    4).
-define(VAL_INTEGER,    5).
-define(VAL_LIT,        6).
-define(VAL_Z,          7). % internal Z register, 4 upper bits are register id

-define(OPCODE_MOVE,        1 + (0 bsl 4)). % move(Src, Dst)
-define(OPCODE_CALL,        1 + (1 bsl 4)). % call(Arity) Z0=CodePtr
-define(OPCODE_TAIL_CALL,   1 + (2 bsl 4)). % tail_call(Arity) Z0=CodePtr
-define(OPCODE_CONS,        1 + (3 bsl 4)).
-define(OPCODE_DECONS,      1 + (4 bsl 4)). % deconstructs list
-define(OPCODE_RET,         1 + (5 bsl 4)).
-define(OPCODE_TEST1,       1 + (6 bsl 4)). % checks jumps to fail, arity 1
-define(OPCODE_TEST2,       1 + (7 bsl 4)). % checks jumps to fail, arity 2

-define(OPCODE_SYSCALL,     2). % call id stored in upper 4 bits
    -define(SYSCALL_ALLOC,          0). % grows a new stack frame
    -define(SYSCALL_DEALLOC,        1). % removes a stack frame
    -define(SYSCALL_RESOLVE_EXPORT, 2). % figures out code location
    -define(SYSCALL_TEST_HEAP,      3). % checks heap/stack size
    -define(SYSCALL_ERROR,          4). % produces error/exception
