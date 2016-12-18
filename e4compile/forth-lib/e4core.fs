:MODULE e4core
:NIF .ERROR -1 ( creates an error exception in the current process )
:NIF .NONVALUE? -2
:NIF .MAKE-MFARITY -3

:NIF .ENTER -10
:NIF .LEAVE -11
:NIF .LD -12
:NIF .ST -13
:NIF .APPLY -14

:NIF == -20
:NIF =:= -21

:NIF .NEW-TUPLE -30
:NIF .SET-ELEMENT -31
:NIF .GET-ELEMENT -32
:NIF .ALLOC-RAW-TUPLE -33
:NIF .IS-TUPLE -34
:NIF .MAKE-TUPLE -35

: ERROR-FN-CLAUSE 'function_clause .ERROR ;
: ERROR-BADMATCH 'badmatch .ERROR ;
: ERROR-BADARG 'badarg .ERROR ;
: ERROR-CASE-CLAUSE 'case_clause .ERROR ;

( OnFail X -- , jumps to OnFail if X is the nonvalue )
( : ON-FAIL-JMP
    .NONVALUE? IF JUMP THEN ; )

