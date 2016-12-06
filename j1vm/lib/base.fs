equ false #h 0000
equ true  #h ffff

: depth dsp h# ff and ;
: 1+ #d 1 + ;
: rot >r swap r> swap ;
: -rot swap >r swap >r ;
: 0= #d 0 = ;
: tuck swap over ;
: 2drop drop drop ;
: 2dup over over ;
: ?dup dup if dup then ;
: +! tuck @ + swap ! ;

: split ( a m -- a&m a&~m ) over and tuck xor ;
: merge ( a b m -- m ? b : a ) >r over xor r> and xor ;

: c@ dup @ swap d# 1 and if d# 8 rshift else h# ff and then ;
: c! swap h# ff and dup d# 8 lshift or swap
	tuck dup @ swap 
	d# 1 and d# 0 = h# ff xor
	merge swap ! ;

: <> = invert ;
: 0<> 0= invert ;
: 0< d# 0 < ;
: 0>= 0< invert ;
: 0> d# 0 \ fallthrough
: > swap < ;
: >= < invert ;
: <= > invert ;
: u> swap u< ;

: negate invert 1+ ;
: - negate + ;
: abs dup 0< if negate then ;
: min 2dup < \ fallthrough
: ?: ( x y f -- x or y ) if drop else nip then ;
: max 2dup > ?: ;
: 2* d# 1 lshift ;
: 2+ d# 2 + ;
: 2- d# 1- 1- ;
: 2/ d# 1 rshift ;
: c+! tuck c@ + swap c! ;

: count dup 1+ swap c@ ;

