%%% @doc Intermediate format library for pass 1
%%% @end
-module(e4_f1).

%% API
-export([
      'and'/1, 'if'/2, 'if'/3, block/0, block/1, block/3, block/4, comment/1,
      comment/2, equals/2, lit/1, match_two_values/2, eval/1,
      store/1, tuple/1, var/1, element/2, unless/2, mark_alias/2,
      mark_new_arg/1, make_mfarity/3, primop/2, include/1, make_tmp/2, emit/2,
      export/2
]).

-include_lib("compiler/src/core_parse.hrl").
-include_lib("e4c/include/forth.hrl").
-include_lib("e4c/include/e4c.hrl").

%% @doc Checks if expr is a simple variable or literal, then returns itself.
%% Creates a tmp variable and assigns the value of Expr to it
make_tmp(Block = #f_block{}, Value) ->
    %% If Value is fully known inside the block we can replace with a temporary
    case j1c_helper:is_value_known(Block, Value) of
        false -> make_tmp(Value); % has only known vars, literals or calls
        true ->
            % value cannot be bound to a temporary
            #{name => Value, forth => []}
    end.

make_tmp(Value) ->
    %% Remove 4 bytes "#Ref" from "#Ref<0.0.2.60>" and prepend "forth"
    <<_:4/binary, TmpName0/binary>> = erlang:list_to_binary(
        erlang:ref_to_list(make_ref())
    ),
    TmpName = <<"forth", TmpName0/binary>>,

    Tmp = var(TmpName),
    TmpCode = [eval(Value), store(Tmp)],
    #{name => Tmp, forth => TmpCode}.


%% @doc Takes list of Forth checks and creates forth instructions which produce
%% true if all conditions are true. Assumption: each Cond in Conds is a Forth
%% sequence which leaves one value on stack
'and'([]) -> [];
'and'(Conds) ->
    %% Remove true clauses
    Conds1 = lists:filter(
        fun(#c_literal{val='true'}) -> false;
            ([]) -> false;
            (_) -> true
        end,
        Conds),
    case Conds1 of
        [] -> [];
        _ -> [Conds1, lists:duplicate(length(Conds1) - 1, 'AND')]
    end.

%% TODO separate Cond from the rest of the arguments. How to optimize if true?
'if'(X, Body) when is_list(Body) -> 'if'(X, block(Body));
'if'(#k_literal{val='true'}, Body = #f_block{}) ->
    Body;
'if'(Cond, Body = #f_block{}) ->
    block(
        [comment("begin if"), Cond, <<"IF">>],
        [Body],
        [<<"THEN">>, comment("end if")]
    ).

'if'(Cond, Body = #f_block{}, Else = #f_block{}) ->
    block(
        [comment("begin ifelse"), Cond, <<"IF">>],
        [Body, <<"ELSE">>, Else],
        [<<"THEN">>]
    ).

unless(#k_literal{val='false'}, _Block) -> [];
unless(Cond, Body = #f_block{}) ->
    block(
        [comment("begin unless"), Cond, <<"UNLESS">>],
        [Body],
        [<<"THEN">>]
    ).

%% ( c b a N - {a,b,c} , constructs a tuple size N from values on stack )
%% Takes list of Forth expressions where each leaves one value on stack
%% and constructs a tuple of that size
tuple(Values) ->
    [
        lists:reverse(lists:map(fun eval/1, Values)),
        lit(length(Values)),
        <<".MAKE-TUPLE">>
    ].

cons(H, T) -> [eval(H), eval(T), <<".CONS">>].

%% ( X Y -- (X==Y) , takes 2 values from stack, pushes comparison result )
equals(Lhs, Rhs) ->
    [eval(Lhs), eval(Rhs), <<"==">>].

%% ( X Y -- X , if X==Y, otherwise badmatch error )
match_two_values(L, R) ->
    [   % TODO: move to core.fs
        eval(L), <<"DUP">>, eval(R), <<"==">>,
        <<"UNLESS">>, <<"ERROR-BADMATCH">>, <<"THEN">>
    ].

%% ( -- Value , leaves a literal value on stack )
%%lit([]) -> #k_nil{};
lit(Value) when is_integer(Value) -> #k_int{val=Value};
lit(Value) when is_atom(Value) -> #k_atom{val=Value};
lit(Value) when is_float(Value) -> #k_float{val=Value};
lit(Value) -> #k_literal{val=Value}.

comment(Str) -> comment("~s", [Str]).

comment(Format, Args) ->
    Txt = iolist_to_binary(io_lib:format(Format, Args)),
    #f_comment{comment=Txt}.

block() -> block([], [], [], []).
block(Code) -> block([], Code, [], []).

-spec block(Before :: forth_ic(),
            Code :: forth_ic(),
            After :: forth_ic()) -> f_block().
block(Before, Code, After) ->
    block(Before, Code, After, []).

-spec block(Before :: forth_ic(),
            Code :: forth_ic(),
            After :: forth_ic(), [k_var()]) ->
    f_block().
block(Before, Code, After, Scope) ->
    #f_block{before=Before, code=Code, aftr=After, scope=Scope}.

%% @doc Emit a structure which later will produce code to store value into
%% the variable, allocated somewhere on the stack
store([]) -> []; % for where empty ret=[] is provided
store([Dst = #k_var{}]) -> #f_st{var=var(Dst)};
store(Dst = #k_var{}) -> #f_st{var=var(Dst)}.

%% @doc If both args are variables, creates an alias for the next compiler
%% pass and generates no code. Otherwise generates code for copying.
mark_alias(Var = #k_var{}, Existing = #k_var{}) ->
    #f_var_alias{var=var(Var), existing=Existing};
mark_alias(Existing = #k_var{}, #f_stacktop{}) ->
    [<<"DUP">>, store(Existing)].

%% @doc Produce code which will evaluate or retrieve variable value and leave
%% it on stack for the code that follows.
eval(#f_stacktop{})         -> [];
eval(Retr = #f_ld{})        -> Retr;
eval(#k_var{name={F, A}}) when is_atom(F), is_integer(A) ->
    #k_local{name=F, arity=A};
eval(#k_var{name=Var})      -> #f_ld{var=Var};
eval(#k_tuple{es=Elements}) -> tuple(Elements);
eval(#k_cons{hd=H, tl=T})   -> cons(H, T);

eval(Lit = #k_local{}) -> Lit;  % local fun-arity
eval(Lit = #k_remote{}) -> Lit; % external mod-fun-arity
eval(Lit = #k_literal{}) -> Lit;
eval(Lit = #k_atom{}) -> Lit;
eval(Lit = #k_float{}) -> Lit;
eval(Lit = #k_int{}) -> Lit;
eval(Lit = #k_nil{}) -> Lit;
eval(Code) when is_list(Code) -> Code;
eval(Other) -> ?COMPILE_ERROR("Eval ~s - unknown element",
                              [?COLOR_TERM(red, Other)]).

var(#k_var{name=Name})          -> var(Name);
var(Name) when is_binary(Name)  -> #k_var{name=Name};
var(Name) when is_atom(Name)    -> #k_var{name=atom_to_binary(Name, utf8)}.

%mark_new_var(#c_var{}=V) -> #f_decl_var{var=var(V)};
%%mark_new_var(L) when is_list(L) -> lists:map(fun mark_new_var/1, L);
%%mark_new_var(#k_var{} = V) -> #f_decl_var{var=var(V)};
%%mark_new_var(Name) -> #f_decl_var{var=var(Name)}.

mark_new_arg(#k_var{} = V) -> #f_decl_arg{var=var(V)}.

element(Index, Tuple) ->
    [eval(Tuple), eval(Index), ?F_GETELEMENT].

make_mfarity('.', F, Arity) when is_atom(F) ->
    #k_local{name=F, arity=Arity};
make_mfarity(M, F, Arity) when is_atom(M), is_atom(F) ->
    #k_remote{mod=M, name=F, arity=Arity};
make_mfarity(MExpr, FExpr, Arity) ->
    [MExpr, FExpr, lit(Arity), <<".MAKE-MFARITY">>].

primop(#c_literal{val=Primop}, Arity) -> primop(Primop, Arity);
primop(match_fail, 1) ->
    <<"ERROR-CASE-CLAUSE">>;
primop(Name, Arity) ->
    ?COMPILE_ERROR("Unknown primop ~p/~p", [Name, Arity]).

include(F) -> #f_include{filename=F}.

%% @doc Add code to the end of the #f_block{}
-spec emit(Block :: f_block(), Code :: forth_ic()) -> f_block().
emit(Block = #f_block{}, AddCode) when not is_list(AddCode) ->
    emit(Block, [AddCode]);
emit(Block = #f_block{}, AddCode) ->
    lists:foldl(fun emit_x_into_y/2, Block, AddCode).

emit_x_into_y(#k_var{}, _Blk) ->
    ?COMPILE_ERROR("should not emit variable");
emit_x_into_y(Nested, Blk) when is_list(Nested) ->
    emit(Blk, Nested);
%% Append when code is just #f_block{}
%%emit_x_into_y(ForthOp, Blk = #f_block{code=Code}) when is_tuple(Code) ->
%%    Blk#f_block{code=[Code] ++ [ForthOp]};
%% Append when code is a list
emit_x_into_y(ForthOp, Blk = #f_block{code=Code}) ->
    Blk#f_block{code=Code ++ [ForthOp]}.

export(F, Arity) ->
    #f_export{fn=atom_to_binary(F, utf8), arity=Arity}.
