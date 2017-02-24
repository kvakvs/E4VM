%% @doc ecpp AST builder API. Creates a tree of Erlang records representing a
%% C++ program or a module. To render text representation see ecpp_render.
%% @end

-module(ecpp_ast).

-include_lib("ecpp/include/ecpp_ast.hrl").

-export([
      assign/2,
      block/0, block/1, block/3, block/4,
      call/2,
      comment/1, comment/2,
      function/4,
      fun_name/2, fun_name/3,
      if_block/2, if_block/3,
      literal/1,
      module/2,
      ref/1,
      return/1,
      string/1, string/2,
      tail_call/2,
      try_block/2
    ]).

ref(X) -> #cpp_ref{ref = X}.

block() -> block([], [], [], []).
block(Code) -> block([], Code, [], []).
block(Bef, Body, Aft) -> block(Bef, Body, Aft, []).

block(Bef, Body, Aft, Scope) ->
    #cpp_block{before = Bef,
               code = Body,
               aftr = Aft,
               scope = Scope}.

%% @doc Emit a structure which later will produce code to store value into
%% the variable, allocated somewhere on the stack
assign([], Rhs) -> Rhs; % for where empty ret=[] is provided
assign([Dst = #cpp_var{}], Rhs) -> #cpp_assign{lhs = Dst, rhs = Rhs};
assign(Dst = #cpp_var{}, Rhs) -> #cpp_assign{lhs = Dst, rhs = Rhs}.

comment(T) ->
    #cpp_comment{ comment = maybe_as_binary(T) }.

comment(Format, Args) -> #cpp_comment{comment = fmt(Format, Args)}.

try_block(Code, Catches) ->
    block(#cpp_try{},
        Code,
        #cpp_catch{clauses = Catches}).

call(Target0, Args) ->
    Target = maybe_as_binary(Target0),
    case is_value_literal(Target) of
        true -> #cpp_call{target = Target, args = Args};
        false -> #cpp_call{target = <<"apply">>, args = [Target, Args]}
    end.

tail_call(What, Args) ->
    #cpp_call{target = maybe_as_binary(What), args = Args, tailcall = true}.

return(Val) ->
    #cpp_return{val = Val}.

function(Type, Name, Args, Code) ->
    #cpp_fun{ type = maybe_as_binary(Type),
              name = maybe_as_binary(Name),
              args = Args,
              code = Code }.

if_block(X, Body) when is_list(Body) ->
    if_block(X, block(Body));
if_block(Cond, Body = #cpp_block{}) ->
    if_block(Cond, Body, []).

if_block(Cond, Body = #cpp_block{}, Else) ->
    #cpp_if{condition = Cond,
            true = Body,
            false = Else}.

module(Name, Body) ->
    #cpp_module{name = Name, body = Body}.

literal(X) -> #cpp_lit{val = X}.

fun_name(#cpp_lit{val = Name}, Arity) -> fun_name(Name, Arity);
fun_name(Name, Arity) when is_atom(Name) ->
    fmt("~s_~B", [stringify_if_atom(Name), Arity]).

fun_name(#cpp_lit{val = Mod0}, #cpp_lit{val = Name0}, Arity) ->
    Mod = stringify_if_atom(Mod0),
    Name = call_rename(Mod, stringify_if_atom(Name0)),
    fmt("~s::~s_~B", [Mod, Name, Arity]).

string(S) -> fmt("\"~s\"", [S]).

string(Format, Args) -> fmt("\"" ++ Format ++ "\"", Args).

%%%-----------------------------------------------------------------------------

maybe_as_binary(B) when is_binary(B) -> B;
maybe_as_binary(S) when is_list(S) -> erlang:list_to_binary(S);
maybe_as_binary(A) when is_atom(A) -> erlang:atom_to_binary(A, utf8);
maybe_as_binary(#cpp_var{name = Var}) -> Var.

fmt(Format, Args) ->
    erlang:iolist_to_binary(io_lib:format(Format, Args)).

%% @doc Check if value is literally known at the compile time
is_value_literal(A) when is_atom(A) -> true;
is_value_literal(B) when is_binary(B) -> true;
is_value_literal(#cpp_lit{}) -> true;
is_value_literal(_) -> false.

stringify_if_atom(A) when is_atom(A) -> erlang:atom_to_binary(A, utf8);
stringify_if_atom(#cpp_lit{val = A}) when is_atom(A) -> maybe_as_binary(A);
stringify_if_atom(L) when is_list(L) -> erlang:list_to_binary(L);
stringify_if_atom(Other) -> Other.

call_rename(<<"erlang">>, F) -> call_rename_erlang(F);
call_rename(M, F) ->
    io:format("call_rename ~p ~p~n", [M, F]),
    F.

call_rename_erlang(<<"=:=">>) -> <<"equals_strict">>;
call_rename_erlang(<<"==">>) -> <<"equals">>;
call_rename_erlang(<<">=">>) -> <<"greater_than_eq">>;
call_rename_erlang(<<"=<">>) -> <<"less_than_eq">>;
call_rename_erlang(<<">">>) -> <<"greater_than">>;
call_rename_erlang(<<"<">>) -> <<"less_than">>;
call_rename_erlang(F) -> F.
