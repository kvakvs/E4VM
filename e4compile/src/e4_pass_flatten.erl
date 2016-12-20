%%% @doc 1. Unroll Intermediate Code and helper records into a flat Forth
%%% program consisting of 'words' and 'literals' only.
%%% 2. Emit the code to allocate and free stack frames.

-module(e4_pass_flatten).

%% API
-export([process/1]).
-include("e4_forth.hrl").
-include("e4.hrl").

process(IC) ->
    Out = lists:flatten(transform(#f_var_storage{}, IC, [])),
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
     stack_frame_enter(length(Vars#f_var_storage.stack_frame)),
     transform(e4_f2:merge(Vars, ParentScope), Code, []),
     stack_frame_leave(length(Vars#f_var_storage.stack_frame))
    ];
transform(ParentScope = #f_var_storage{}, [<<":">> | T], Out) ->
    [Out,
     <<":">>,
%%     stack_frame(ParentScope, T),
        transform(ParentScope, T, [])
    ];
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
    e4_forth_parse:parse(F),
    e4_f1:comment("end include ~s", [F])
];
transform_op(_Scope, X) -> X.

stack_frame(Scope, Code) ->
    %% Count nested #f2_blocks, union their variables and produce a stack
    %% frame enter instruction that has enough space to fit all of them
    [].

stack_frame_enter(0) -> [];
stack_frame_enter(Sz) when is_integer(Sz) -> #f_enter{size=Sz}.

stack_frame_leave(0) -> [];
stack_frame_leave(Sz) when is_integer(Sz) -> #f_leave{size=Sz}.
