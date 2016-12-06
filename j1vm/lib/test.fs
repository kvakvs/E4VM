\ 
\ In general, using tests is simple:
\ { actions -> result }
\ 
\ E.g.: { 1 2 + -> 3 } or { 1 2 + 3 4 + -> 3 7 }
\ 

var test-sp
var test-stack

equ ADDR_DUMP 32767 \ 0x7fff

: dump ADDR_DUMP @ drop ;

: test-pass 0 ADDR_DUMP ! ;
: test-fail ADDR_DUMP ! ;

: { test-stack test-sp ! ;

: -> 
	 begin 
		test-sp @ !
		test-sp @ 2 + test-sp !
	dsp 0 = until
	; 

: } 
	 dsp test-sp @ 1 rshift 1 - = if \ compate depth and test-sp
	 	test-stack >r 
		begin 
			r@ @ \ fetch address
			= if else \ fail if not equals to the stack top
				2 test-fail ;
			then
			r> 2 + >r \ go to the next cell
		dsp 0 = until
		r> drop
		test-pass
	 else
			1 test-fail ;
	 then
	;

