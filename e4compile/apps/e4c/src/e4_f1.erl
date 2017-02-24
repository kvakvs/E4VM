%%% @doc Intermediate format library for pass 1
%%% @end
-module(e4_f1).

%% API
-export([
    'and'/1,
    comment/1, comment/2,
    element/2,
    equals/2,
    eval/1,
    export/2,
    include/1,
    is_in_the_scope/2,
    lit/1,
    make_mfarity/3,
    make_tmp/1,
    mark_new_arg/1,
    primop/2,
    tuple/1,
    var/1
]).

-include_lib("compiler/src/core_parse.hrl").
-include_lib("e4c/include/forth.hrl").
-include_lib("e4c/include/e4c.hrl").
-include_lib("ecpp/include/ecpp_ast.hrl").


%% @doc Consult with scope and find out if value contains only known parts
%% such as bound variables and literals, or some dynamic parts such as calls
%% and (free) unbound variables. Having a known or dynamic value allows
%% binding it to a temporary before some other calculation.
%%-spec is_value_known(cpp_block(), any()) -> boolean().
%%is_value_known(Block, Elements) when is_list(Elements) ->
%%    lists:all(fun(E) -> is_value_known(Block, E) end,
%%              Elements);
%%is_value_known(_Block, #k_literal{}) -> true;
%%is_value_known(_Block, #k_int{})     -> true;
%%is_value_known(_Block, #k_float{})   -> true;
%%is_value_known(_Block, #k_atom{})    -> true;
%%is_value_known(_Block, #k_nil{})     -> true;
%%is_value_known(Block = #cpp_block{}, #k_tuple{es=Elements}) ->
%%    is_value_known(Block, Elements);
%%is_value_known(Block = #cpp_block{}, #k_cons{hd=H, tl=T}) ->
%%    is_value_known(Block, H) andalso is_value_known(Block, T);
%%is_value_known(Block = #cpp_block{}, #cpp_block{code=Code}) ->
%%    is_value_known(Block, Code);
%%%%is_value_known(_Block, #cpp_var{})     -> true; % assuming var exists
%%is_value_known(#cpp_block{scope=Scope}, #cpp_var{} = Var) ->
%%    is_in_the_scope(Scope, Var).

%% @doc Given a scope from an #f_block{} and a #cpp_var{} checks if the var exists
%% in that scope (bound) or doesn't (free)
is_in_the_scope(Scope, #cpp_var{name=Name}) ->
    lists:member(Name, Scope).

%% @doc Checks if expr is a simple variable or literal, then returns itself.
%% Creates a tmp variable and assigns the value of Expr to it
-spec make_tmp(any())
              -> #{name => binary() | any(), code => cpp_code()}.
make_tmp(Value) ->
    %% Remove 4 bytes "#Ref" from "#Ref<0.0.2.60>" and prepend "forth"
    <<_:5/binary, TmpName0/binary>> = erlang:list_to_binary(
        erlang:ref_to_list(make_ref())
    ),
    TmpName1 = [ <<(maketmp_variable_name_char(X)):8>> || <<X>> <= TmpName0],
    TmpName = erlang:iolist_to_binary(["tmp_", TmpName1]),

    Tmp = var(TmpName),
    TmpCode = ecpp_ast:assign(Tmp, eval(Value)),
    #{name => Tmp, code => TmpCode}.

maketmp_variable_name_char(X)
    when X >= $0 andalso X =< $9
         orelse X >= $A andalso X =< $Z
         orelse X >= $a andalso X =< $z -> X;
maketmp_variable_name_char(_Y) -> $_.

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

%% ( c b a N - {a,b,c} , constructs a tuple size N from values on stack )
%% Takes list of Forth expressions where each leaves one value on stack
%% and constructs a tuple of that size
tuple(Values) ->
    ecpp_ast:call("make_tuple", lists:map(fun eval/1, Values)).

cons(H, T) ->
    ecpp_ast:call("cons", [eval(H), eval(T)]).

%% ( X Y -- (X==Y) , takes 2 values from stack, pushes comparison result )
equals(Lhs, Rhs) ->
    [ecpp_ast:call("equals", [eval(Lhs), eval(Rhs)])].

%% ( -- Value , leaves a literal value on stack )
lit(Value) when is_integer(Value) ->
    ecpp_ast:literal(Value);

lit(#k_atom{val = Value}) -> ecpp_ast:literal(Value);
lit(Value) when is_atom(Value) -> ecpp_ast:literal(Value);
%%    ecpp_ast:call("make_atom", [ecpp_ast:string(Value)]);

lit(Value) when is_float(Value) -> #k_float{val=Value};
lit(Value) -> #k_literal{val=Value}.

comment(Str) -> comment("~s", [Str]).

comment(Format, Args) ->
    Txt = iolist_to_binary(io_lib:format(Format, Args)),
    #f_comment{comment=Txt}.

%% @doc Produce code which will evaluate or retrieve variable value and leave
%% it on stack for the code that follows.
eval(#f_stacktop{}) -> [];
eval(Retr = #f_ld{}) -> Retr;
eval(#cpp_var{name = {F, A}})
    when is_atom(F), is_integer(A) ->
    #k_local{name = F, arity = A};
eval(#k_var{} = KVar) -> eval(var(KVar));
eval(#k_tuple{es = Elements}) -> tuple(Elements);
eval(#k_cons{hd = H, tl = T}) -> cons(H, T);

eval(#cpp_var{} = Var) -> Var;
eval(#cpp_call{} = Call) -> Call;

eval(#k_local{name = F, arity = A}) ->
    ecpp_ast:fun_name(F, A);
eval(#k_remote{mod = M, name = F, arity = A}) ->
    ecpp_ast:fun_name(eval(M), eval(F), A);
eval(#k_literal{val = Lit}) -> ecpp_ast:literal(Lit);
eval(#k_atom{val = Atom}) -> ecpp_ast:literal(Atom);
eval(#k_float{val = Flt}) -> ecpp_ast:literal(Flt);
eval(#k_int{val = Int}) -> ecpp_ast:literal(Int);
eval(#k_nil{}) -> ecpp_ast:literal([]);
eval(Code) when is_list(Code) -> Code;
eval(Other) -> ?COMPILE_ERROR("Eval ~s - unknown element",
                              [?COLOR_TERM(red, Other)]).

var([#k_var{name = Name}]) -> var(Name);
var(#k_var{name = Name}) -> var(Name);
var(#cpp_var{} = V) -> V;
var(Name) when is_binary(Name) -> #cpp_var{name = Name};
var(Name) when is_atom(Name) -> #cpp_var{name = atom_to_binary(Name, utf8)}.

mark_new_arg(#cpp_var{} = V) -> #f_decl_arg{var=var(V)}.

element(Index, Tuple) ->
    ecpp_ast:call("element", [eval(Tuple), lit(Index)]).

make_mfarity('.', F, Arity) when is_atom(F) ->
    #k_local{name = F, arity = Arity};
make_mfarity(M, F, Arity) when is_atom(M), is_atom(F) ->
    #k_remote{mod = M, name = F, arity = Arity};
make_mfarity(MExpr, FExpr, Arity) ->
    [MExpr, FExpr, lit(Arity), <<".MAKE-MFARITY">>].

primop(#c_literal{val = Primop}, Arity) -> primop(Primop, Arity);
primop(match_fail, 1) ->
    <<"ERROR-CASE-CLAUSE">>;
primop(Name, Arity) ->
    ?COMPILE_ERROR("Unknown primop ~p/~p", [Name, Arity]).

include(F) -> #f_include{filename=F}.

export(F, Arity) ->
    #f_export{fn = atom_to_binary(F, utf8), arity = Arity}.
