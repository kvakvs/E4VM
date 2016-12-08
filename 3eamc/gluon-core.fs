: ERROR-FN-CLAUSE 'function_clause' ERROR ;

: TUPLE0 ( -- , returns {} )
    0 .NEW-TUPLE ;

: TUPLE1 ( a -- , returns {a} )
    1 .NEW-TUPLE            ( -- a Tuple )
    DUP 0                   ( -- a Tuple Tuple 0 )
    .SET-ELEMENT            ( -- a Tuple )
    SWAP DROP ;             ( -- Tuple )

: TUPLE ( a b c ... N -- , loop 1..N )
    DUP .ALLOC-RAW-TUPLE    ( a b c N N -- a b c N Tuple , create empty )
    SWAP 0 DO               ( a b c N Tuple -- a b c Tuple N 0 , begin loop 0..N-1 )
        I .SET-ELEMENT      ( populate inplace with elements reversed on stack )
    LOOP
    ;

: TRIM ( ... N -- , drops N args from the stack )
    0 DO SWAP DROP LOOP ;

: ON-FAIL-JMP ( OnFail X -- , jumps to OnFail if X is the nonvalue )
    NONVALUE? IF JUMP THEN ;
