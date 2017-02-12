%%% @doc Takes a list of #j1*{} records and binary words and converts it to
%%% J1 bytecode. Addresses for jump and similar are written as labels, and
%%% the caller (j1c_pass_link) is responsible for resolving addresses to offsets
%%% and encoding them properly into a long or short jump.

-module(j1c_compile_bin).

%% API
-export([compile_segment/2]).

-include_lib("e4c/include/forth.hrl").
-include_lib("e4c/include/e4c.hrl").
-include_lib("j1c/include/j1.hrl").
-include_lib("j1c/include/j1bytecode.hrl").

-spec compile_segment(j1prog(), j1forth_code())
                     -> #{p => j1prog(), bin => j1compiled()}.
compile_segment(Prog0 = #j1prog{}, Input) ->
    %% Setup compiler state (ignored after finished)
    Prog1 = Prog0#j1prog{pc = 0},

    Prog2 = process_words(Prog1, Input),

    %% Linking/compiling is done, we can flatten the list and optimize
    Bin1 = lists:reverse(lists:flatten(Prog2#j1prog.output)),
    Bin2 = j1c_optimize:optimize(Bin1, []),
    #{ p => Prog2#j1prog{output = []},
       bin => #j1compiled{bin = Bin2} }.

%%%-----------------------------------------------------------------------------

-spec process_words(j1prog(), j1forth_code()) -> j1prog().
process_words(Prog0 = #j1prog{}, []) -> Prog0;

process_words(Prog0, [OpList | Tail]) when is_list(OpList) ->
    Prog1 = lists:foldl(fun(Op, P0) -> process_words(P0, [Op]) end,
                        Prog0,
                        OpList),
    process_words(Prog1, Tail);

process_words(Prog0 = #j1prog{}, [?F_RET | Tail]) ->
    Prog1 = emit_alu(Prog0, #j1alu{op = ?J1ALU_T, rpc = 1, ds = 2}),
    process_words(Prog1, Tail);

process_words(Prog0 = #j1prog{}, [?F_LIT_NIL | Tail]) ->
    Prog1 = emit(Prog0, j1c_bc:literal_nil()),
    process_words(Prog1, Tail);

%% Nothing else worked, look for the word in our dictionaries and base words,
%% maybe it is a literal, too
process_words(Prog0 = #j1prog{}, [Int | Tail])
    when is_integer(Int) ->
        ProgA = emit(Prog0, j1c_bc:literal_integer(Int)),
        process_words(ProgA, Tail);

process_words(Prog0, [#j1comment{} | Tail]) ->
    process_words(Prog0, Tail);

process_words(Prog0, [#j1atom{id = AtomId} | Tail]) ->
    %% TODO: Add bits to mark immediate atoms, ints etc or an arbitrary literal
    Prog1 = emit(Prog0, j1c_bc:literal_atom(AtomId)),
    process_words(Prog1, Tail);

process_words(Prog0, [#j1lit{id = LitId} | Tail]) ->
    Prog1 = emit(Prog0, j1c_bc:literal_arbitrary(LitId)),
    process_words(Prog1, Tail);

process_words(Prog0, [#j1ld{index = Index} | Tail]) ->
    Prog1 = emit(Prog0, j1c_bc:load(Index)),
    process_words(Prog1, Tail);

process_words(Prog0, [#j1st{index = Index} | Tail]) ->
    Prog1 = emit(Prog0, j1c_bc:store(Index)),
    process_words(Prog1, Tail);

process_words(Prog0, [#j1getelement{index = Index} | Tail]) ->
    Prog1 = emit(Prog0, j1c_bc:get_element(Index)),
    process_words(Prog1, Tail);

process_words(Prog0, [#j1enter{size = Size} | Tail]) ->
    Prog1 = emit(Prog0, j1c_bc:enter(Size)),
    process_words(Prog1, Tail);

%% LEAVE and LEAVE;RET
process_words(Prog0, [#j1leave{}, ?F_RET | Tail]) ->
    Prog1 = emit(Prog0, j1c_bc:leave()),
    process_words(Prog1, Tail);
process_words(Prog0, [#j1leave{} | Tail]) ->
%%    Prog1 = emit(Prog0, j1c_bc:leave()),
    ?COMPILE_ERROR("Stray LEAVE without RET following it"),
    process_words(Prog0, Tail);

process_words(Prog0, [#j1erl_call{lit = _Lit} | Tail]) ->
    Prog1 = emit(Prog0, j1c_bc:erl_call()),
    process_words(Prog1, Tail);
process_words(Prog0, [#j1erl_tailcall{lit = _Lit} | Tail]) ->
    Prog1 = emit(Prog0, j1c_bc:erl_tail_call()),
    process_words(Prog1, Tail);

process_words(Prog0, #j1jump{condition = Cond, label = F}) ->
    Op = case Cond of
             false  -> j1c_bc:jump_signed(F);
             z      -> j1c_bc:jump_z_signed(F)
         end,
    emit(Prog0, Op);

process_words(Prog0 = #j1prog{pc = PC, labels = Labels, lpatches = Patch},
              [#j1label{label = F} | Tail]) ->
    Labels1 = orddict:store(F, PC, Labels),
    Prog1 = Prog0#j1prog{
        labels = Labels1,
        lpatches = [PC | Patch]
    },
    process_words(Prog1, Tail);

process_words(Prog0 = #j1prog{}, [Word | Tail]) ->
%% Possibly a word, try resolve
    Prog1 = case prog_find_word(Prog0, Word) of
                not_found -> emit_base_word(Prog0, Word);
                Index -> emit_call(Prog0, Index)
            end,
    process_words(Prog1, Tail).

%%process_words(_Prog, [Word | _Tail]) ->
%%    ?COMPILE_ERROR1("Word is unexpected", Word).


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
    emit(Prog0, j1c_bc:call_signed(Index)).

%%emit(Prog0 = #j1bin{output=Out, pc=PC}, #j1patch{}=Patch) ->
%%    Prog0#j1bin{output=[Patch | Out],
%%                 pc=PC + 1};
-spec emit(j1prog(), binary() | [binary()]) -> j1prog().
emit(Prog0 = #j1prog{output=Out, pc=PC}, IOList) ->
    Prog0#j1prog{output=[IOList | Out],
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
    emit(Prog, j1c_bc:alu(ALU)).

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