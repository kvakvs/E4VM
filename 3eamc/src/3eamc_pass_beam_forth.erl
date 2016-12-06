-module('3eamc_pass_beam_forth').
-include("3eamc.hrl").

%% API
-export([process/2]).
-import('3eamc_encode', [varint/1, val_int/1, val_zreg/1, val_xreg/1]).

process([], Accum) ->
    lists:reverse(Accum);
process([Item | Tail], Accum) ->
    process(Tail, [transform_fn(Item) | Accum]).

transform_fn({function, _Name, _Arity, _Label, Code}) ->
    transform_fn_code(Code, []).

%% Processes stream of BEAM instructions. Also processes sequences of instructs
transform_fn_code([], Accum) -> lists:reverse(Accum);
transform_fn_code([In = {put_tuple, _, _} | Tail], Accum) ->
    io:format("--> in ~p~n", [In]),
    transform_fn_sequence_put_tuple(In, Tail, Accum);
transform_fn_code([In | Tail], Accum) ->
    io:format("--> in ~p~n", [In]),
    Out = transform_one(In),
    io:format("  <-out ~p~n", [Out]),
    transform_fn_code(Tail, [Out | Accum]).

%% Takes {put_tuple...} and a sequence of puts that follows it, extracts them
%% and emits an instruction which builds a tuple
transform_fn_sequence_put_tuple({put_tuple, N, Dst}, Code, Accum) ->
    IsAPut = fun({put, _}) -> true; (_) -> false end,
    {Puts0, Tail} = lists:splitwith(IsAPut, Code),
    Puts1 = lists:map(fun({put, Val}) -> Val end, Puts0),
    OutCode = lists:reverse(Puts1) ++ [N, 'MAKE-TUPLE', f_emit_write(Dst)],
    io:format("  <-out ~p~n", [OutCode]),
    transform_fn_code(Tail, [OutCode | Accum]).

transform_one({label, L}) -> {label, L};
transform_one({line, _Ln}) -> [];
transform_one({func_info, _M, {atom, Name}, Arity}) ->
    [{comment, {function, Name, Arity}}, 'ERROR-FN-CLAUSE'];
transform_one({badmatch, X}) -> f_emit_error(badmatch, X);
transform_one({allocate_zero, A, B}) -> transform_one({allocate, A, B});
transform_one({allocate, A, B}) -> [A, B, 'ALLOC'];
transform_one({deallocate, A}) -> [A, 'DEALLOC'];
transform_one({move, Src, Dst}) -> f_emit_move(Src, Dst);
transform_one({call, Arity, {f,_} = Label}) -> [Label, Arity, 'CALL'];
transform_one({call_only, Arity, Label}) -> [Label, Arity, 'TAIL-CALL'];
transform_one({call_last, Arity, Label, Dealloc}) ->
    transform_one({deallocate, Dealloc})
    ++ transform_one({call_only, Arity, Label});
transform_one({call_ext, Arity, MFArity}) ->
    [MFArity, 'RESOLVE-MFA', Arity, 'CALL']; % TODO: optimize me?
transform_one({call_ext_only, Arity, MFArity}) ->
    [MFArity, 'RESOLVE-MFA', Arity, 'TAIL-CALL'];
transform_one({call_ext_last, Arity, MFArity, Dealloc}) ->
    transform_one({deallocate, Dealloc})
    ++ transform_one({call_ext_only, Arity, MFArity});
transform_one({call_fun, Fun}) -> [f_emit_read(Fun), 'CALL-FUN'];
%% Args go in x[0]..x[Arity-1], module goes in x[Arity], fun in x[Arity+1]
transform_one({apply, Arity}) -> [Arity, 'APPLY'];
%% Args go in x[0]..x[Arity-1], module goes in x[Arity], fun in x[Arity+1]
transform_one({apply_last, Arity, _Something}) -> [Arity, 'TAIL-APPLY'];
transform_one({test_heap, A, B}) -> [B, A, 'TEST-HEAP'];
transform_one({put_list, Head, Tail, Dst}) ->
    [f_emit_read(Head), f_emit_read(Tail), 'CONS', f_emit_write(Dst)];
%%transform_one({put_tuple, _N, Dst}) ->
%%    {'_op_make_tuple', Dst};
%%transform_one({put, Val}) ->
%%    {'_op_tuple_append', Val};
%%transform_one({'_op_tuple_put', Val}) ->
%%    Val;
transform_one({kill, {y, Y}}) -> [Y, 'KILL-Y'];
transform_one(return) -> 'EXIT';
transform_one({jump, L}) -> [L, 'JUMP'];
transform_one({get_list, H, T, List}) ->
    [f_emit_read(List),
     'GET-LIST',
     f_emit_write(T),
     f_emit_write(H)];
transform_one({case_end, X}) -> f_emit_error(case_clause, X);
transform_one({test, Predicate, LFail, Args}) ->
    lists:reverse(lists:map(fun f_emit_read/1, Args))
    ++ [f_emit_predicate(Predicate), 'IF', LFail, 'JUMP', 'THEN'];
transform_one({get_tuple_element, Src, Element, Dst}) ->
    [f_emit_read(Src),
     f_emit_read(Element),
     'ELEMENT',
     f_emit_write(Dst)];
transform_one({gc_bif, Name, OnFail, Live, Args, Reg}) ->
    asm_bif(Name, OnFail, Live, Args, Reg);
transform_one({select_val, Value, OnFail, {list, Choices}}) ->
    {'_op_select_val', Value, OnFail, Choices};
transform_one({make_fun2, Label, _Index, _OldUniq, NumFree}) ->
    %% BEAM: this op takes a preparsed (at load time) FunEntry and should
    %% produce a callable object. We have to invent something simpler.
    [NumFree, Label, '*MAKE-FUN2'];
transform_one({trim, N, _Remaining}) ->
    %% BEAM: Reduce the stack usage by N words keeping the CP on the top.
    [N, 'TRIM'];
transform_one({bif, Name, OnFail, Args, Dst}) ->
    [OnFail]
    ++ lists:reverse(lists:map(fun f_emit_read/1, Args))
    ++ [Name, f_emit_write(Dst), 'ON-FAIL-JMP']; % TODO a library function?
transform_one(Other) ->
    io:format("Unsupported opcode ignored: ~p~n", [Other]),
    erlang:error({unsupported_opcode, ?MODULE, Other}).

f_emit_error(E) ->
    [f_emit_read(E), 'ERROR'].

f_emit_error(Type, Value) ->
    [f_emit_read(Type), f_emit_read(Value), 'ERROR/2'].

asm_bif(Name, OnFail, Live, Args, Reg) ->
    %% Call the bif Bif with the argument Arg, and store the result in Reg.
    %% On fail jump to OnFail. Do a GC if necessary to allocate
    %% space on the heap for the result (saving Live number of X registers).
    {'_op_bif', Name, OnFail, Live, Args, Reg}.

%% Given value or register or slot reference emits Forth instruction to put
%% this value onto stack
f_emit_read(nil)                    -> 'NIL';
f_emit_read(N) when is_integer(N)   -> N;
f_emit_read({integer, N})           -> N;
f_emit_read({x, X})                 -> [X, 'READ-X'];
f_emit_read({y, Y})                 -> [Y, 'READ-Y'];
f_emit_read({f, Label})             -> {label, Label};
f_emit_read(A) when is_atom(A)      -> f_emit_read({atom, A});
f_emit_read({atom, A})              -> {atom, A};
f_emit_read({literal, L})           -> {literal, L};
f_emit_read({extfunc, M, F, Arity}) -> {extfunc, M, F, Arity}.
%%f_emit_read({imm, L}) ->
%%    case classify_value(L) of
%%        {ok, Type} ->
%%            f_emit_read({Type, L});
%%        false -> % encode as a literal
%%            f_emit_read({literal, L})
%%    end;

f_emit_write({x, X}) -> [X, 'WRITE-X'];
f_emit_write({y, Y}) -> [Y, 'WRITE-Y'].

f_emit_move({x, Src}, {x, Dst}) -> [Dst, Src, 'MOVE-XX'];
f_emit_move({x, Src}, {y, Dst}) -> [Dst, Src, 'MOVE-XY'];
f_emit_move({y, Src}, {x, Dst}) -> [Dst, Src, 'MOVE-YX'];
f_emit_move(Src, Dst)           -> [f_emit_read(Src), f_emit_write(Dst)].

f_emit_predicate(test_arity)       -> 'TEST-ARITY/2';
f_emit_predicate(is_tuple)         -> 'IS-TUPLE';
f_emit_predicate(is_eq_exact)      -> 'IS-EQ-EXACT/2';
f_emit_predicate(is_nil)           -> 'IS-NIL';
f_emit_predicate(is_nonempty_list) -> 'IS-NONEMPTY-LIST';
f_emit_predicate(is_integer)       -> 'IS-INTEGER';
f_emit_predicate(is_atom)          -> 'IS-ATOM';
f_emit_predicate(is_function2)     -> 'IS-FUN/2'; % is_function2 Lbl Arg1 Arity
f_emit_predicate(is_float)         -> 'IS-FLOAT';
f_emit_predicate(is_list)          -> 'IS-LIST';
f_emit_predicate(is_binary)        -> 'IS-BINARY';
f_emit_predicate(is_ge)            -> '>=';
f_emit_predicate(is_gt)            -> '>';
f_emit_predicate(is_le)            -> '=<';
f_emit_predicate(is_lt)            -> '<';
f_emit_predicate(X) ->
    erlang:error({unknown_predicate, X}).