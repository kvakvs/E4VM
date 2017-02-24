%%% @doc Print the nice indented colored intermediate code
%%% @end

-module(e4c_print).

-include_lib("e4c/include/forth.hrl").
-include_lib("e4c/include/e4c.hrl").

%% API
-export([format_ic/2]).

format_ic(L, Indent) when is_list(L) ->
    [format_ic(Item, Indent) || Item <- L];
format_ic(#cpp_block{before=B, scope=_S, code=C, aftr=A}, Indent) ->
    [format_ic(B, Indent + 1),
     format_ic(C, Indent + 1),
     format_ic(A, Indent + 1)];
format_ic(#f2_block{code=C}, Indent) ->
    format_ic(C, Indent + 1);
format_ic(C, Indent) ->
    io_lib:format("~s~s~n", [i(Indent), format_op(C)]).

format_op(#f_apply{funobj=FO, args=Args}) ->
    io_lib:format("~s(~s;~s)", [color:whiteb("apply"), format_op(FO),
                                [format_op(A) || A <- Args]]);
format_op(W) when is_atom(W) ->
    io_lib:format("~s", [color:whiteb(str(W))]);
format_op(W) when ?IS_FORTH_WORD(W) ->
    io_lib:format("~s", [color:whiteb(str(W))]);

format_op(#k_nil{}) ->
    io_lib:format("lit:[]", []);
format_op(#k_literal{val=L}) ->
    io_lib:format("lit:~s", [color:magenta(str(L))]);
format_op(#k_atom{val=A}) ->
    io_lib:format("atom:~s", [color:white(str(A))]);
format_op(#k_int{val=A}) ->
    io_lib:format("int:~s", [color:redb(str(A))]);
format_op(#k_float{val=A}) ->
    io_lib:format("float:~s", [color:magentab(str(A))]);

format_op(#f_enter{size=S}) ->
    io_lib:format("~s(~p)", [color:yellow("ENTER"), S]);
format_op(#f_leave{size=S}) ->
    io_lib:format("~s(~p)", [color:yellow("LEAVE"), S]);
format_op(#f_ld{var=#k_var{name=V}}) ->
    io_lib:format("~s(~s)", [color:green("retrieve"), format_op(V)]);
format_op(#f_st{var=#k_var{name=V}}) ->
    io_lib:format("~s(~s)", [color:red("store"), format_op(V)]);

format_op(#f2_ld{index=V}) ->
    io_lib:format("~s(~p)", [color:green("LD"), V]);
format_op(#f2_st{index=V}) ->
    io_lib:format("~s(~p)", [color:red("ST"), V]);

format_op(#f_decl_var{var=#k_var{name=V}}) ->
    io_lib:format("~s(~s)", [color:yellow("decl-var"), format_op(V)]);
format_op(#f_decl_arg{var=#k_var{name=V}}) ->
    io_lib:format("~s(~s)", [color:yellow("decl-arg"), format_op(V)]);
format_op(#f_var_alias{var=V, existing=Alt}) ->
    io_lib:format("~s(~s=~s)", [
        color:blackb("alias"), format_op(V), format_op(Alt)]);
format_op(#f_comment{comment=C}) ->
    io_lib:format("~s ~s",
                  [color:blackb("\\"), color:blackb(C)]);
format_op(#k_tuple{es=Elements}) ->
    Ops = [format_op(E) || E <- Elements],
%%    Ops1 = [[Op, ", "] || Op <- Ops],
    Ops1 = string:join(Ops, ", "),
    io_lib:format("tuple:{~s}", [Ops1]);
format_op(#k_cons{hd=H, tl=T}) ->
    io_lib:format("cons:[~s | ~s]", [format_op(H), format_op(T)]);
format_op(#k_local{name=N, arity=A}) ->
    io_lib:format("funarity:~s/~p", [color:whiteb(str(N)), A]);
format_op(#k_remote{mod=M, name=F, arity=A}) ->
    io_lib:format("mfarity:~s,~s,~s", [format_op(M), format_op(F), str(A)]);
format_op(#f_include{filename=F}) ->
    io_lib:format("~s(~s)", [color:whiteb("include"), F]);

format_op(#f_export{fn=N, arity=A}) ->
    io_lib:format("~s(~s/~p)", [color:redb("EXPORT"), color:whiteb(str(N)), A]);

format_op(#k_var{name=Var}) -> Var; % binary
format_op(Other) ->
    ?COMPILE_ERROR("format_op: unknown ~p", [Other]).

str(X) when is_atom(X) -> atom_to_list(X);
str(X) when is_binary(X) -> io_lib:format("~s", [X]);
str({A, B}) when is_atom(A), is_integer(B) ->
    io_lib:format("~s/~p", [A, B]);
str(X) -> lists:flatten(io_lib:format("~p", [X])).

i(I) when I =< 0 -> [];
i(I) -> lists:duplicate((I-1) * 4, 32).
