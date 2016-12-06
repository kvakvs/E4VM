-module('3eamc_pass_beam_asm').
-include("3eamc.hrl").

%% API
-export([
    transform/1
]).
-import('3eamc_encode', [varint/1, val_int/1, val_zreg/1, val_xreg/1]).

%% Encodes one tuple from code (in the compiled BEAM)
%% This function is used for single BEAM instructions. Sequences of BEAM
%% instructions are TODO process sequences in the outer loop (write_code)
transform({function, _Name, _Arity, _Label, Code}) ->
    TransformedCode = [begin
                           io:format("-- transforming ~p...~n", [I]),
                           T = transform_one(I),
                           io:format("  -> ~p~n", [T]),
                           T
                       end || I <- Code],
    _TransformedCode1 = lists:flatten(TransformedCode).
    % {'#fun', {atom, Name}, Arity} | TransformedCode1].

transform_one({label, L}) -> {'#label', L};
transform_one({line, _Ln}) -> [];
%%transform_one({line, Ln}) -> {'#ln', Ln};
transform_one({func_info, _Mod, _Fun, _Arity}) ->
    asm_error(function_clause);
transform_one({badmatch, X}) ->
    asm_error(badmatch);
transform_one({allocate_zero, A, B}) ->
    transform_one({allocate, A, B});
transform_one({allocate, A, B}) ->
    {'_op_alloc', A, B};
transform_one({deallocate, A}) ->
    {'_op_dealloc', A};
transform_one({move, Src, Dst}) ->
    asm_move(Src, Dst);
transform_one({call, Arity, {f,_} = Label}) ->
    asm_call(label, '_op_call', {Label, Arity});
transform_one({call_only, Arity, Label}) ->
    asm_call(label, '_op_tail_call', {Label, Arity});
transform_one({call_last, Arity, Label, Dealloc}) ->
    [
        transform_one({deallocate, Dealloc}),
        transform_one({call_only, Arity, Label})
    ];
transform_one({call_ext, _Arity, MFArity}) ->
    asm_call(mfarity, '_op_call', MFArity);
transform_one({call_ext_only, _Arity, MFArity}) ->
    asm_call(mfarity, '_op_tail_call', MFArity);
transform_one({apply, Arity}) ->
    %% Args go in x[0]..x[Arity-1], module goes in x[Arity], fun in x[Arity+1]
    asm_apply('_op_call', Arity);
transform_one({apply_last, Arity, _}) ->
    %% Args go in x[0]..x[Arity-1], module goes in x[Arity], fun in x[Arity+1]
    asm_apply('_op_tail_call', Arity);
transform_one({test_heap, A, B}) ->
    {'_op_test_heap', A, B};
transform_one({put_list, Head, Tail, Dst}) ->
    {'_op_cons', Head, Tail, Dst};
transform_one({put_tuple, N, Dst}) ->
    {'_op_make_tuple', Dst};
transform_one({put, Val}) ->
    {'_op_tuple_append', Val};
transform_one({'_op_tuple_put', Val}) ->
    Val;
transform_one(return) ->
    {'_op_ret'};
transform_one({jump, L}) ->
    {'_op_jump', L};
transform_one({get_list, H, T, List}) ->
    {'_op_decons', H, T, List};
transform_one({case_end, X}) ->
    asm_error(case_clause, X);
transform_one({test, Predicate, LFail, Args}) ->
    case Args of
        [Arg1] -> {'_op_test1', Predicate, LFail, Arg1};
        [Arg1, Arg2] -> {'_op_test2', Predicate, LFail, Arg1, Arg2}
    end;
transform_one({get_tuple_element, Src, Element, Dst}) ->
    {'_op_element', Src, Element, Dst};
transform_one({gc_bif, Name, OnFail, Live, Args, Reg}) ->
    asm_bif(Name, OnFail, Live, Args, Reg);
transform_one({select_val, Value, OnFail, {list, Choices}}) ->
    {'_op_select_val', Value, OnFail, Choices};
transform_one(Other) ->
    io:format("Unsupported opcode ignored: ~p~n", [Other]),
    erlang:error({unsupported_opcode, ?MODULE}).

%% A,B must be encoded value or encoded destination, not an integer
asm_move(A, B) when not is_integer(A), not is_integer(B) ->
    {'_op_move', A, B}.

asm_syscall(Id) when Id >= 0 andalso Id =< 15 ->
    {'_op_syscall', Id}.

asm_call(label = _DestKind, CallOpcode, {{f,_}=Label, Arity})
    when is_atom(CallOpcode) ->
    %% Just load label address (resolved at load-time) and then call
    [
        asm_move(Label, {z, 0}),
        {CallOpcode, Arity}
    ];
asm_call(mfarity = _DestKind, CallOpcode, {extfunc, Mod, Fun, Arity})
    when is_atom(CallOpcode) ->
    {'_op_call_mfarity', Mod, Fun, Arity};
asm_call(DestKind, CallOpcode, Arg) ->
    io:format("Err: asm_call(~p, ~p, ~p)~n", [DestKind, CallOpcode, Arg]),
    erlang:error({transformation_error, ?MODULE}).

%% Args go in x[0]..x[Arity-1], module goes in x[Arity], fun in x[Arity+1]
asm_apply(ApplyOpcode, Arity) when is_atom(ApplyOpcode) ->
    {'_op_apply', Arity}.

asm_error(E) ->
    {'_op_error', E, nil}.
asm_error(Type, Value) ->
    {'_op_error', Type, Value}.

asm_bif(Name, OnFail, Live, Args, Reg) ->
    %% Call the bif Bif with the argument Arg, and store the result in Reg.
    %% On fail jump to OnFail. Do a GC if necessary to allocate
    %% space on the heap for the result (saving Live number of X registers).
    {'_op_bif', Name, OnFail, Live, Args, Reg}.
