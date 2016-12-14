-module(e4_cf).

%% API
-export(['and'/1, 'if'/2, 'if'/3, block/0, block/1, block/3, block/4, comment/1,
    comment/2, equals/2, lit/1, match_two_values/2, nil/0, retrieve/1, store/1,
    tuple/1, var/1, element/2, unless/2, mark_alias/2, mark_new_var/1, mark_new_arg/1]).

-include_lib("compiler/src/core_parse.hrl").
-include("e4_cf.hrl").

%% Takes list of Forth checks and creates forth instructions which produce
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
'if'(#cf_lit{val='true'}, Body = #cf_block{}) ->
    Body;
'if'(Cond, Body = #cf_block{}) ->
    block([comment("begin if"), Cond, 'IF'], [Body], ['THEN']).

'if'(Cond, Body = #cf_block{}, Else = #cf_block{}) ->
    block([comment("begin ifelse"), Cond, 'IF'],
        [Body, 'ELSE', Else],
        ['THEN']).

unless(#cf_lit{val='false'}, _Block) -> [];
unless(Cond, Body = #cf_block{}) ->
    block([comment("begin unless"), Cond, 'UNLESS'], [Body], ['THEN']).

%% ( c b a N - {a,b,c} , constructs a tuple size N from values on stack )
%% Takes list of Forth expressions where each leaves one value on stack
%% and constructs a tuple of that size
tuple(Values) ->
    [
        lists:reverse(lists:map(fun retrieve/1, Values)),
        lit(length(Values)),
        'MAKE-TUPLE'
    ].

%% ( X Y -- (X==Y) , takes 2 values from stack, pushes comparison result )
equals(Lhs, Rhs) -> [Lhs, Rhs, '=='].

%% ( X Y -- X , if X==Y, otherwise badmatch error )
match_two_values(L, R) ->
    [retrieve(L), 'DUP', retrieve(R), '==',
     'UNLESS', 'BADMATCH', 'THEN'].

%% ( -- Value , leaves a literal value on stack )
lit(Value) -> #cf_lit{val=Value}.

comment(Str) -> comment("~s", [Str]).

comment(Format, Args) ->
    Txt = iolist_to_binary(io_lib:format(Format, Args)),
    #cf_comment{comment=Txt}.

%% ( -- nil , leaves value [] on stack )
nil() -> 'NIL'.

block() -> block([], [], [], []).
block(Code) -> block([], Code, [], []).

-spec block(cf_code(), cf_code(), cf_code()) -> cf_block().
block(Before, Code, After) ->
    block(Before, Code, After, []).

-spec block(cf_code(), cf_code(), cf_code(), [cf_var()]) ->
    cf_block().
block(Before, Code, After, Scope) ->
    #cf_block{before=Before, code=Code, 'after'=After, scope=Scope}.

%% ( X -- , stores value X on stack into variable Dst )
store(Dst = #cf_var{}) -> #cf_store{var=Dst}.

%% @doc If both args are variables, creates an alias for the next compiler
%% pass and generates no code. Otherwise generates code for copying.
mark_alias(Var = #cf_var{}, Existing = #cf_var{}) ->
    #cf_alias{var=Var, existing=Existing};
mark_alias(Existing = #cf_var{}, #cf_stack_top{}) ->
    ['DUP', store(Existing)].

%% ( -- X , retrieves value of variable V and leaves it on stack )
retrieve(#c_tuple{es=Es}) -> tuple(Es);
retrieve(#c_apply{op=FunObj, args=Args}) ->
    #cf_apply{funobj=FunObj, args=Args};
retrieve(#cf_apply{}=A) -> A;
retrieve(I) when is_integer(I) -> lit(I);
retrieve(#cf_stack_top{}) -> [];
retrieve(Retr = #cf_retrieve{}) -> Retr;
retrieve(Lit = #cf_lit{}) -> Lit;
retrieve(#c_literal{val=Lit}) -> lit(Lit);
retrieve(#c_var{name={F, A}}) when is_atom(F), is_integer(A) ->
    #cf_mfarity{mod='.', fn=F, arity=A};
retrieve(#c_var{name=Var}) -> #cf_retrieve{var=Var};
retrieve(Var = #cf_var{}) -> #cf_retrieve{var=Var}.

var(#c_var{name=Name}) -> #cf_var{name=Name};
var(#cf_var{}=CF) -> CF;
var(Name) -> #cf_var{name=Name}.

mark_new_var(#c_var{}=V) -> #cf_new_var{var=var(V)};
mark_new_var(#cf_var{}=V) -> #cf_new_var{var=var(V)};
mark_new_var(Name) -> #cf_new_var{var=var(Name)}.

mark_new_arg(#cf_var{}=V) -> #cf_new_arg{var=var(V)}.

element(Index, Tuple) ->
    [retrieve(Tuple), retrieve(Index), 'ELEMENT'].
