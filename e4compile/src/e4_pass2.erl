%%% @doc Compile Core Forth AST into Forth program consisting of words
%%% and literals. Optimize. Resolve variables.

-module(e4_pass2).

%% API
-export([process/1]).
-include("e4_forth.hrl").
-import(e4, [compile_error/2, compile_error/1]).

process(CoreForth) ->
    NewM = #f_module{
        cfgraph = digraph:new([cyclic, protected])
    },
    #f_module{output=F} = process_code(NewM, CoreForth),
    io:format("PASS2~n~p~n", [F]),
    F.

process_code(Mod0 = #f_module{output=Code}, []) ->
    Mod0#f_module{output=lists:flatten(Code)};
process_code(Mod0 = #f_module{}, [CF | Tail]) ->
    Mod1 = process_code(Mod0, CF),
    process_code(Mod1, Tail);
process_code(Mod0, Op) -> % if a single item is given, like a root block
    process_op(Mod0, Op).

-spec emit(Mod :: f_module(), Code :: intermediate_forth_code()) -> f_block().
%%emit(_Mod0, #f_ld{}) -> compile_error("E4 Pass2: can't emit LD construct");
%%emit(_Mod0, #f_st{}) -> compile_error("E4 Pass2: can't emit ST construct");
emit(Mod0, AddCode) when not is_list(AddCode) ->
    emit(Mod0, [AddCode]);
emit(Mod0, AddCode) ->
    lists:foldl(
        fun(Nested, Md) when is_list(Nested) ->
                emit(Md, Nested);
            (ForthOp, Md = #f_module{output=Code}) ->
                Md#f_module{output=Code ++ [ForthOp]}
        end,
        Mod0,
        AddCode).

stack_frame_enter(Mod0, 0) -> Mod0;
stack_frame_enter(Mod0, Sz) when is_integer(Sz) ->
    emit(Mod0, #f_enter{size=Sz}).

stack_frame_leave(Mod0, 0) -> Mod0;
stack_frame_leave(Mod0, Sz) when is_integer(Sz) ->
    emit(Mod0, #f_leave{size=Sz}).

%% Atoms processing
process_op(Mod0 = #f_module{}, A) when ?IS_FORTH_WORD(A) ->
    emit(Mod0, A); % pass through forth words

%% Other code structures processing
process_op(Mod0 = #f_module{scope=OuterScope},
           #f_block{before=Before, code=Code,
               'after'=After, scope=InnerScope}) ->
    %% Enter the scope
    EnterScope = ordsets:union([InnerScope, OuterScope]),
    FrameSize  = length(InnerScope),
    Mod10      = stack_frame_enter(Mod0, FrameSize),
    Mod20 = Mod10#f_module{scope=EnterScope},

    Mod30 = process_code(Mod20, [Before, Code, After]),

    %% Restore scope
    Mod40 = stack_frame_leave(Mod30, FrameSize),
    Mod40#f_module{scope=OuterScope};

process_op(Mod0 = #f_module{}, #f_var_alias{var=V, existing=E}) ->
    add_alias(Mod0, V, E);
process_op(Mod0 = #f_module{}, #f_decl_var{var=V}) ->
    allocate_var(Mod0, V);
process_op(Mod0 = #f_module{}, #f_decl_arg{var=V}) ->
    declare_arg(Mod0, V);
process_op(Mod0 = #f_module{}, #f_comment{} = C) ->
    emit(Mod0, C);
process_op(Mod0 = #f_module{}, #f_mfa{} = MFA) ->
    emit(Mod0, MFA);
process_op(Mod0 = #f_module{}, #f_ld{var=V}) ->
    emit(Mod0, e4_f2:retrieve(Mod0, V));
process_op(Mod0 = #f_module{}, #f_st{var=V}) ->
    emit(Mod0, e4_f2:store(Mod0, V));
process_op(Mod0 = #f_module{}, #f_lit{} = Lit) ->
    emit(Mod0, Lit);
process_op(Mod0, #f_include{filename=F}) ->
    emit(Mod0, e4_forth_parse:parse(F));
process_op(Mod0 = #f_module{}, #f_apply{funobj=FO, args=Args}) ->
    Mod1 = lists:foldl(
        fun(Arg, M0) -> emit(M0, e4_f2:retrieve(M0, Arg)) end,
        Mod0,
        Args),
    emit(Mod1, [e4_f2:retrieve(Mod1, FO), <<"APPLY">>]);
process_op(_Mod0, CF) ->
    compile_error("E4 Pass2: Unknown op ~p~n", [CF]).

allocate_var(Mod0 = #f_module{alloc_vars=Vars}, V) ->
    Mod0#f_module{alloc_vars=e4_f2:alloc_var(Vars, V, stack_frame)}.

declare_arg(Mod0 = #f_module{alloc_vars=Vars}, V) ->
    Mod0#f_module{alloc_vars=e4_f2:alloc_var(Vars, V, pre_existing)}.

add_alias(Mod0 = #f_module{alloc_vars=Vars}, NewV, ExistingV) ->
    Mod0#f_module{alloc_vars=e4_f2:add_alias(Vars, NewV, ExistingV)}.
