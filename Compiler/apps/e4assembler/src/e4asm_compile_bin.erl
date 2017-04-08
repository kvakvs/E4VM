%%% @doc Takes a list of #j1*{} records and binary words and converts it to
%%% J1 bytecode. Addresses for jump and similar are written as labels, and
%%% the caller (j1c_pass_link) is responsible for resolving addresses to offsets
%%% and encoding them properly into a long or short jump.

-module(e4asm_compile_bin).

%% API
-export([compile_segment/2]).

-include_lib("e4compiler/include/forth.hrl").
-include_lib("e4compiler/include/e4c.hrl").
-include_lib("e4assembler/include/j1.hrl").
-include_lib("e4assembler/include/j1bytecode.hrl").

-spec compile_segment(j1prog(), j1forth_code())
                     -> #{p => j1prog(), bin => list(binary())}.
compile_segment(Prog0 = #j1prog{pc = Pc0}, Input) ->
    %% Setup compiler state (ignored after finished)
    Prog1 = Prog0#j1prog{output = []},

    Prog2 = compile_many(Prog1, Input),

    %% Linking/compiling is done, we can flatten the list and optimize
    Bin1 = lists:reverse(lists:flatten(Prog2#j1prog.output)),
%%    Bin2 = j1c_optimize:optimize(Bin1, []),

    Dis = e4asm_dis:disasm(Prog2, Pc0, iolist_to_binary(Bin1)),
    io:format("~s~n", [Dis]),

    #{ p => Prog2#j1prog{output = []}, bin => Bin1 }.

%%%-----------------------------------------------------------------------------

-spec compile_many(j1prog(), j1forth_code()) -> j1prog().
compile_many(Prog0 = #j1prog{}, []) -> Prog0;

compile_many(Prog0, [OpList | Tail]) when is_list(OpList) ->
    Prog1 = lists:foldl(fun(Op, P0) -> compile_many(P0, [Op]) end,
                        Prog0,
                        OpList),
    compile_many(Prog1, Tail);

%% LEAVE;RET. Single LEAVE is handled in compile_1 and produces an error
compile_many(Prog0, [#j1leave{}, ?F_RET | Tail]) ->
    Prog1 = emit(Prog0, e4asm_bytecode:leave()),
    compile_many(Prog1, Tail);

compile_many(Prog0, [Word | Tail]) ->
    Prog1 = compile_1(Prog0, Word),
    compile_many(Prog1, Tail);

%% Handle a non-list (single op)
compile_many(Prog0, Op) when not is_list(Op) ->
    compile_1(Prog0, Op).

%%%-----------------------------------------------------------------------------

compile_1(Prog0 = #j1prog{}, ?F_RET) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_T, rpc = 1, ds = 2});

compile_1(Prog0 = #j1prog{}, ?F_LIT_NIL) ->
    emit(Prog0, e4asm_bytecode:literal_nil());

%% Nothing else worked, look for the word in our dictionaries and base words,
%% maybe it is a literal, too
compile_1(Prog0 = #j1prog{}, Int) when is_integer(Int) ->
    emit(Prog0, e4asm_bytecode:literal_integer(Int));

compile_1(Prog0, #j1comment{}) -> Prog0;

compile_1(Prog0, #j1atom{id = AtomId}) ->
    %% TODO: Add bits to mark immediate atoms, ints etc or an arbitrary literal
    emit(Prog0, e4asm_bytecode:literal_atom(AtomId));

compile_1(Prog0, #j1lit{id = LitId}) ->
    emit(Prog0, e4asm_bytecode:literal_arbitrary(LitId));

compile_1(Prog0, #j1ld{index = Index}) ->
    emit(Prog0, e4asm_bytecode:load(Index));

compile_1(Prog0, #j1st{index = Index}) ->
    emit(Prog0, e4asm_bytecode:store(Index));

compile_1(Prog0, #j1getelement{index = Index}) ->
    emit(Prog0, e4asm_bytecode:get_element(Index));

compile_1(Prog0, #j1enter{size = Size}) ->
    emit(Prog0, e4asm_bytecode:enter(Size));

%%compile_1(_Prog0, #j1leave{}) ->
%%    ?COMPILE_ERROR("Stray LEAVE without RET following it");

compile_1(Prog0, #j1erl_call{lit = _Lit}) ->
    emit(Prog0, e4asm_bytecode:erl_call());

compile_1(Prog0, #j1erl_tailcall{lit = _Lit}) ->
    emit(Prog0, e4asm_bytecode:erl_tail_call());

compile_1(Prog0, #j1jump{condition = Cond, label = F}) ->
    Op = case Cond of
             false  -> e4asm_bytecode:jump_signed(F);
             z      -> e4asm_bytecode:jump_z_signed(F)
         end,
    emit(Prog0, Op);

compile_1(Prog0 = #j1prog{pc = PC, labels = Labels, lpatches = Patch},
          #j1label{label = F}) ->
    Labels1 = orddict:store(F, PC, Labels),
    Prog0#j1prog{
        labels = Labels1,
        lpatches = [PC | Patch]
    };

compile_1(Prog0 = #j1prog{}, Word) ->
    %% Possibly a word, try resolve
    case prog_find_word(Prog0, Word) of
        not_found -> emit_base_word(Prog0, Word);
        Index -> emit_call(Prog0, Index)
    end.

%%%-----------------------------------------------------------------------------

%% @doc Looks up a word in the dictionary, returns its address or 'not_found'
-spec prog_find_word(j1prog(), forth_word()) -> integer() | not_found.
prog_find_word(#j1prog{dict_nif = NifDict, dict = Dict},
               Word) when is_binary(Word) ->
    case orddict:find(Word, NifDict) of
        {ok, Index1} -> Index1; % nifs have negative indexes
        error ->
            case orddict:find(Word, Dict) of
                {ok, Index2} -> Index2;
                error -> not_found
            end
    end.

%% @doc Emits a CALL instruction with Label Index (signed) into the code.
%% Negative indices point to NIF functions. Label indexes are resolved to
%% relative offsets or addresses and possibly the value bits are extended in
%% a later pass.
emit_call(Prog0 = #j1prog{}, Index) ->
    emit(Prog0, e4asm_bytecode:call_signed(Index)).

%%emit(Prog0 = #j1bin{output=Out, pc=PC}, #j1patch{}=Patch) ->
%%    Prog0#j1bin{output=[Patch | Out],
%%                 pc=PC + 1};
-spec emit(j1prog(), binary() | [binary()]) -> j1prog().
emit(Prog0, Elements) when is_list(Elements) ->
    lists:foldl(fun(Elem, Prg) -> emit(Prg, Elem) end, Prog0, Elements);
emit(Prog0 = #j1prog{output=Out, pc=PC}, Bin) when is_binary(Bin) ->
    Prog0#j1prog{output=[Bin | Out],
                 pc=PC + 1}.

%%%-----------------------------------------------------------------------------

-spec '_emit_alu_fold_helper'(j1alu(), j1prog()) -> j1prog().
'_emit_alu_fold_helper'(ALU, JBin) ->
    emit_alu(JBin, ALU).

-spec emit_alu_f(j1prog(), [j1alu()]) -> j1prog().
emit_alu_f(Prog = #j1prog{}, ALUList) ->
    lists:foldl(fun '_emit_alu_fold_helper'/2, Prog, ALUList).

-spec emit_alu(j1prog(), j1alu()) -> j1prog().
emit_alu(Prog = #j1prog{}, ALU = #j1alu{ds=-1}) ->
    emit_alu(Prog, ALU#j1alu{ds=2});
emit_alu(Prog = #j1prog{}, ALU = #j1alu{rs=-1}) ->
    emit_alu(Prog, ALU#j1alu{rs=2});
emit_alu(Prog = #j1prog{}, ALU = #j1alu{}) ->
    emit(Prog, e4asm_bytecode:alu(ALU)).

%%%-----------------------------------------------------------------------------

-spec emit_base_word(j1prog(), j1forth_code()) -> j1prog().
%%
%% Special stuff and folding
%%
emit_base_word(Prog0, L) when is_list(L) ->
    lists:foldl(fun(Op, P0) -> emit_base_word(P0, Op) end, Prog0, L);

%%
%% Words
%%
emit_base_word(Prog0, <<"+">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_T_PLUS_N, ds = -1});
emit_base_word(Prog0, <<"XOR">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_T_XOR_N, ds = -1});
emit_base_word(Prog0, <<"AND">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_T_AND_N, ds = -1});
emit_base_word(Prog0, <<"OR">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_T_OR_N, ds = -1});

emit_base_word(Prog0, <<"INVERT">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_INVERT_T});

emit_base_word(Prog0, <<"=">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_N_EQ_T, ds = -1});
emit_base_word(Prog0, <<"<">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_N_LESS_T, ds = -1});
emit_base_word(Prog0, <<"U<">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_N_UNSIGNED_LESS_T, ds = -1});

emit_base_word(Prog0, <<"SWAP">>) -> % swap data stack top 2 elements
    emit_alu(Prog0, #j1alu{op = ?J1ALU_N, tn = 1});
emit_base_word(Prog0, <<"DUP">>) -> % clone data stack top
    emit_alu(Prog0, #j1alu{op = ?J1ALU_T, tn = 1});
emit_base_word(Prog0, <<"DROP">>) -> % drop top on data stack
    emit_alu(Prog0, #j1alu{op = ?J1ALU_N, ds = -1});
emit_base_word(Prog0, <<"OVER">>) -> % clone second on data stack
    emit_alu(Prog0, #j1alu{op = ?J1ALU_N, tn = 1, ds = 1});
emit_base_word(Prog0, <<"NIP">>) -> % drops second on data stack
    emit_alu(Prog0, #j1alu{op = ?J1ALU_T, ds = -1});

emit_base_word(Prog0, <<">R">>) -> % place onto Rstack
    emit_alu(Prog0, #j1alu{op = ?J1ALU_N, tr = 1, ds = -1, rs = 1});
emit_base_word(Prog0, <<"R>">>) -> % take from Rstack
    emit_alu(Prog0, #j1alu{op = ?J1ALU_R, tn = 1, ds = 1, rs = -1});
emit_base_word(Prog0, <<"R@">>) -> % read Rstack top
    emit_alu(Prog0, #j1alu{op = ?J1ALU_R, tn = 1, ds = 1, rs = 0});
emit_base_word(Prog0, <<"@">>) -> % read address
    emit_alu(Prog0, #j1alu{op = ?J1ALU_INDEX_T});
emit_base_word(Prog0, <<"!">>) -> % write address
    emit_alu_f(Prog0, [
        #j1alu{op = ?J1ALU_T, ds = -1},
        #j1alu{op = ?J1ALU_N, ds = -1}
    ]);

emit_base_word(Prog0, <<"DSP">>) -> % get stack depth
    emit_alu(Prog0, #j1alu{op = ?J1ALU_DEPTH, tn = 1, ds = 1});

emit_base_word(Prog0, <<"LSHIFT">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_N_LSHIFT_T, ds = -1});
emit_base_word(Prog0, <<"RSHIFT">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_N_RSHIFT_T, ds = -1});
emit_base_word(Prog0, <<"1-">>) -> % decrement stack top
    emit_alu(Prog0, #j1alu{op = ?J1ALU_T_MINUS_1});
emit_base_word(Prog0, <<"2R>">>) ->
    emit_alu_f(Prog0, [
        #j1alu{op = ?J1ALU_R, tn = 1, ds = 1, rs = -1},
        #j1alu{op = ?J1ALU_R, tn = 1, ds = 1, rs = -1},
        #j1alu{op = ?J1ALU_N, tn = 1}
    ]);
emit_base_word(Prog0, <<"2>R">>) ->
    emit_alu_f(Prog0, [
        #j1alu{op = ?J1ALU_N, tn = 1},
        #j1alu{op = ?J1ALU_N, tr = 1, ds = -1, rs = 1},
        #j1alu{op = ?J1ALU_N, tr = 1, ds = -1, rs = 1}
    ]);
emit_base_word(Prog0, <<"2R@">>) ->
    emit_alu_f(Prog0, [
        #j1alu{op = ?J1ALU_R, tn = 1, ds = 1, rs = -1},
        #j1alu{op = ?J1ALU_R, tn = 1, ds = 1, rs = -1},
        #j1alu{op = ?J1ALU_N, tn = 1, ds = 1},
        #j1alu{op = ?J1ALU_N, tn = 1, ds = 1},
        #j1alu{op = ?J1ALU_N, tr = 1, ds = -1, rs = 1},
        #j1alu{op = ?J1ALU_N, tr = 1, ds = -1, rs = 1},
        #j1alu{op = ?J1ALU_N, tn = 1}
    ]);
emit_base_word(Prog0, <<"DUP@">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_INDEX_T, tn = 1, ds = 1});
emit_base_word(Prog0, <<"DUP>R">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_T, tr = 1, rs = 1});
emit_base_word(Prog0, <<"2DUPXOR">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_T_XOR_N, tn = 1, ds = 1});
emit_base_word(Prog0, <<"2DUP=">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_N_EQ_T, tn = 1, ds = 1});
emit_base_word(Prog0, <<"!NIP">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_T, nti = 1, ds = -1});
emit_base_word(Prog0, <<"2DUP!">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_T, nti = 1});
emit_base_word(Prog0, <<"UP1">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_T, ds = 1});
emit_base_word(Prog0, <<"DOWN1">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_T, ds = -1});
emit_base_word(Prog0, <<"COPY">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_N});

emit_base_word(Prog0, <<"NOOP">>) ->
    emit_alu(Prog0, #j1alu{op = ?J1ALU_T});

emit_base_word(_Prog, Word) ->
    ?COMPILE_ERROR1("Base word is not defined", Word).

%%%-----------------------------------------------------------------------------
