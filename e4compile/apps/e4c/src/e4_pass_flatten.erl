%%% @doc 1. Unroll Intermediate Code and helper records into a flat Forth
%%% program consisting of 'words' and 'literals' only.
%%% 2. Emit the code to allocate and free stack frames.

-module(e4_pass_flatten).

%% API
-export([process/1]).

-include_lib("e4c/include/forth.hrl").
-include_lib("e4c/include/e4c.hrl").

process(IC) ->
    Out0 = lists:flatten(transform(#f_var_storage{}, IC, [])), % flat list of IC
    Out  = lists:flatten(lists:map(fun forthify/1, Out0)), % Convert to Forth

    file:write_file("pass3.txt", iolist_to_binary(io_lib:format("~p", [Out]))),
    io:format("~s~n~s~n",
              [color:on_white(color:black(" PASS 3 Flatten ")),
               e4_print_ic:format_ic(Out, 0)]),
    Out.

%% @doc Given parent scope (to merge with child scopes on descend) and some
%% intermediate code tree, produces a list of forth words suitable for
%% flattening and further compilation with e4_j1c
-spec transform(f_var_storage(), forth_ic(), forth_code()) -> forth_code().
transform(_ParentScope, [], Out) -> Out;
transform(ParentScope = #f_var_storage{},
          _In = #f2_block{code=Code, alloc_vars=Vars}, Out) ->
    [Out,
%%     stack_frame_enter(length(Vars#f_var_storage.stack_frame)),
     transform(e4_f2:merge(Vars, ParentScope), Code, [])
%%     , stack_frame_leave(length(Vars#f_var_storage.stack_frame))
    ];
transform(ParentScope = #f_var_storage{}, [<<":">>, FA | T], Out) ->
    [Out, <<":">>, FA,
        stack_frame_enter(stack_frame_union(ParentScope, T, 0)),
        transform(ParentScope, T, [])
    ];
transform(#f_var_storage{}, <<";">>, Out) ->
    [Out, stack_frame_leave(generic), <<";">>];
transform(#f_var_storage{}, ?F_RET, Out) ->
    [Out, stack_frame_leave(generic), ?F_RET];
transform(ParentScope = #f_var_storage{}, [H|T], Out) ->
    [Out, transform(ParentScope, H, []), transform(ParentScope, T, [])];
transform(#f_var_storage{}, [], Out) -> Out;
transform(ParentScope = #f_var_storage{}, Other, Out) ->
    [Out, transform_op(ParentScope, Other)].

%%transform_op(_Scope, #f_comment{comment=C}) -> [<<"(">>, C, <<")">>];
transform_op(_Scope, #f_decl_arg{}) -> [];
transform_op(Scope, #f_ld{var=Var}) ->
    e4_f2:retrieve(Scope, Var);
transform_op(Scope, #f_st{var=Var}) ->
    e4_f2:store(Scope, Var);
transform_op(_Scope, #f_include{filename=F}) -> [
    e4_f1:comment("begin include ~s", [F]),
    j1c_parse:parse(F),
    e4_f1:comment("end include ~s", [F])
];
%%transform_op(_Scope, X) when is_atom(X) ->
%%    <<"'", (atom_to_binary(X, utf8))/binary>>;
%%transform_op(_Scope, #k_int{val=X}) ->
%%    integer_to_binary(X);
transform_op(_Scope, X) ->
    X.

%% @doc Count nested #f2_blocks, union their variables and produce a stack
%% frame enter instruction that has enough space to fit all of them. Assuming
%% that non-overlapping blocks should be reusing each other's cells by leaving
%% and re-entering (growing) frame a bit.
stack_frame_union(Scope, [], Sum) ->
    Sum + length(Scope#f_var_storage.stack_frame);
stack_frame_union(Scope, [#f2_block{alloc_vars=Scope2, code=Code} | T], Sum) ->
    stack_frame_union(Scope, T, stack_frame_union(Scope2, Code, Sum));
stack_frame_union(Scope, [_ | T], Sum) ->
    stack_frame_union(Scope, T, Sum).

stack_frame_enter(0) -> [];
stack_frame_enter(Sz) when is_integer(Sz) -> % #f_enter{size=Sz}.
    [e4_f1:lit(Sz), ?F_ENTER].

%%stack_frame_leave(0) -> [];
stack_frame_leave(generic) ->
    %% generic leave assumes dynamic stack frame which can be dropped without
    %% knowing its size as a single data block, and collected by GC
    ?F_LEAVE.
%%stack_frame_leave(Sz) when is_integer(Sz) ->
%%    [e4_f1:lit(Sz), ?F_LEAVE].

%% @doc Given a flat list of Forth words and some leftover tuples, convert all
%% to Forth words (binary).
forthify(#k_local{name=N, arity=A}) ->
    [?F_LIT_FUNA, forthify(N), forthify(A)];
forthify(#k_remote{mod=M, name=F, arity=A}) ->
    [?F_LIT_MFA, forthify(M), forthify(F), forthify(A)];
forthify(#f2_ld{index=X}) -> [forthify(X), ?F_LD];
forthify(#f2_st{index=X}) -> [forthify(X), ?F_ST];
forthify(X) when is_integer(X) -> integer_to_binary(X);
forthify(#k_int{val=X}) -> integer_to_binary(X);
forthify(#k_atom{val=X}) -> forthify(X);
forthify(#k_nil{}) -> ?F_LIT_NIL;
forthify(#k_literal{val=[]}) -> ?F_LIT_NIL;
forthify(X) when is_atom(X) -> [?F_LIT_ATOM, atom_to_binary(X, utf8)];
forthify(X) -> X.
