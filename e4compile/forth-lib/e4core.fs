:MODULE e4core
:NIF .ERROR         -1 ( creates an error exception in the current process )
:NIF .MAKE-MFARITY  -2

( ------ stack operations ------ )
:NIF .ENTER -10 ( TODO: maybe allocate frame on dynamic heap? )
:NIF .LEAVE -11 ( TODO: if on dynamic heap maybe just let GC take it )
:NIF .LD    -12
:NIF .ST    -13
:NIF .APPLY -14

( ------ comparison tests ------ )
:NIF ==     -20
:NIF =:=    -21

( ------ tuple stuff ------ )
:NIF .SET-ELEMENT       -30
:NIF .GET-ELEMENT       -31
:NIF .ALLOC-RAW-TUPLE   -32 ( allocate tuple but do not set its cells )
:NIF .MAKE-TUPLE        -33 ( ... N -- Tuple , create from N values on stack )

( ------ type tests ------ )
:NIF .TUPLE?    -40
:NIF .LIST?     -41
:NIF .NIL?      -42
:NIF .ATOM?     -43
:NIF .INT?      -44
:NIF .FLOAT?    -45
:NIF .CONS?     -46
:NIF .NONVALUE? -47

( ------ list stuff ------ )
:NIF .CONS      -50 ( H T -- [H|T] , creates a cons cell )
:NIF .DECONS    -51 ( [H|T] -- H T , deconstructs a cons cell )
:NIF .HD        -52
:NIF .TL        -53

: ERROR-FN-CLAUSE 'function_clause .ERROR ;
: ERROR-BADMATCH 'badmatch .ERROR ;
: ERROR-BADARG 'badarg .ERROR ;
: ERROR-CASE-CLAUSE 'case_clause .ERROR ;

( OnFail X -- , jumps to OnFail if X is the nonvalue )
( : ON-FAIL-JMP
    .NONVALUE? IF JUMP THEN ; )
