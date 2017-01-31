%%% @doc J1-like forth to binary compiler adjusted for Erlang needs with
%%% added types, literals etc.

-module(j1c_pass_forth).

%% API
-export([compile/2]).

-include_lib("e4c/include/forth.hrl").
-include_lib("e4c/include/e4c.hrl").
-include_lib("j1c/include/j1.hrl").

compile(ModuleName, SrcForth) ->
    Prog0 = #j1prog{mod = ModuleName},
    % Module name should always be #0
    {Prog0A, _} = atom_index_or_create(Prog0, atom_to_binary(ModuleName, utf8)),

    Prog1 = compile2(Prog0A, preprocess(SrcForth, [])),

    %% Print the output
    J1Forth = lists:reverse(Prog1#j1prog.output),
    Prog2 = Prog1#j1prog{output = J1Forth},
    %Patched = apply_patches(Output, Prog1#j1prog.patch_table, []),
    %Prog2 = Prog1#j1prog{output=Patched},

    file:write_file("j1c_pass_forth.txt",
                    iolist_to_binary(io_lib:format("~p", [J1Forth]))),

    io:format("~s~n~p~n", [color:redb("J1C PASS 1"), J1Forth]),
    Prog2.

preprocess([], Acc) -> lists:flatten(lists:reverse(Acc));
preprocess([?F_LIT_ATOM, Word | T], Acc) ->
    preprocess(T, [#k_atom{val=Word} | Acc]);
preprocess([H | T], Acc) -> preprocess(T, [H | Acc]).

%%%-----------------------------------------------------------------------------

compile2(Prog0 = #j1prog{}, []) -> Prog0;
compile2(Prog0 = #j1prog{}, [#f_export{fn=Fn, arity=Arity} | Tail]) ->
    Prog1 = prog_add_export(Prog0, Fn, Arity),
    compile2(Prog1, Tail);

%% NEW FUNCTION -- : MFA ... ;
compile2(Prog0 = #j1prog{}, [<<":">>, ?F_LIT_FUNA, Fn, Arity | Tail]) ->
    Prog1 = prog_add_word(Prog0, #k_local{name=Fn, arity=Arity}),
    compile2(Prog1, Tail);

compile2(Prog0 = #j1prog{}, [<<":MODULE">>, Name | Tail]) ->
    compile2(Prog0#j1prog{mod=Name}, Tail);
compile2(Prog0 = #j1prog{}, [<<":">>, Name | Tail]) ->
    Prog1 = prog_add_word(Prog0, Name),
    compile2(Prog1, Tail);
compile2(Prog0 = #j1prog{}, [<<":NIF">>, Name, Index0 | Tail]) ->
    Index1 = binary_to_integer(Index0),
    Prog1 = prog_add_nif(Prog0, Name, Index1),
    compile2(Prog1, Tail);

%% --- Conditions ---
compile2(Prog0 = #j1prog{}, [<<"IF">> | Tail]) ->
    {F, Prog1} = begin_condition(Prog0),
    Prog2 = emit(Prog1, #j1jump{label = F, condition = z}),
    compile2(Prog2, Tail);
compile2(Prog0 = #j1prog{}, [<<"UNLESS">> | Tail]) ->
    compile2(Prog0, [<<"INVERT">>, <<"IF">> | Tail]);
compile2(Prog0 = #j1prog{}, [<<"THEN">> | Tail]) ->
    Prog1 = cond_update_label(Prog0, 0),
    compile2(Prog1, Tail);
compile2(Prog0 = #j1prog{}, [<<"ELSE">> | Tail]) ->
    %% combine IF and THEN - update a label address for previous IF and create
    %% a new label and jump instruction to jump over the code after ELSE
    %% IF[] ----------------> ELSE -------------> THEN[upd patchtable]
    %% #j1patch is emitted here  |                  |
    %% with conditional jump     |                  |
    %%                   patchtable is updated here |
    %%                   jump is emitted here       |
    %%                                             patchtable is updated here
    %%
    %% Pop from the cond table, update label id with current PC
    Prog1 = cond_update_label(Prog0, 1),
    %% Make new label and create a jump to it
    {F, Prog2} = begin_condition(Prog1),
    Prog2 = emit(Prog1, #j1jump{label = F}),
    %% Wait for THEN and do the same again to finalize the condition
    compile2(Prog2, Tail);

%% --- Loops (started with a BEGIN) ---
compile2(Prog0 = #j1prog{}, [<<"BEGIN">> | Tail]) ->
    {_F, Prog1} = begin_loop(Prog0),
    compile2(Prog1, Tail);
compile2(Prog0 = #j1prog{}, [<<"AGAIN">> | Tail]) -> % endless loop
    %% Just emit jump back to the BEGIN instruction
    {Begin, Prog1} = end_loop(Prog0),
    Prog2 = emit(Prog1, #j1jump{label = Begin}),
    compile2(Prog2, Tail);
compile2(Prog0 = #j1prog{}, [<<"UNTIL">> | Tail]) -> % conditional if-zero loop
    %% Emit conditional jump back to the BEGIN instruction
    {Begin, Prog1} = end_loop(Prog0),
    Prog2 = emit(Prog1, #j1jump{label = Begin}),
    compile2(Prog2, Tail);

%% TODO: EQU, maybe VAR, ARR?

compile2(Prog0 = #j1prog{}, [?F_LIT_MFA, M, F, A | Tail]) -> % mfa literal
    Prog1 = emit_lit(Prog0, mfa, {M, F, A}),
    compile2(Prog1, Tail);
compile2(Prog0 = #j1prog{}, [?F_LIT_FUNA, Fn, Arity | Tail]) ->
    Prog1 = emit_lit(Prog0, funarity, {Fn, Arity}),
    compile2(Prog1, Tail);
compile2(Prog0 = #j1prog{}, [#k_atom{val = A} | Tail]) ->
    Prog1 = emit_lit(Prog0, atom, A),
    compile2(Prog1, Tail);
compile2(Prog0 = #j1prog{}, [#k_literal{val = L} | Tail]) ->
    Prog1 = emit_lit(Prog0, arbitrary, L),
    compile2(Prog1, Tail);

%% Comment - pass through
compile2(Prog0 = #j1prog{}, [C = #f_comment{} | Tail]) ->
    compile2(emit(Prog0, [C]), Tail);
%% A binary, probably a word? Pass through
compile2(Prog0 = #j1prog{}, [Bin | Tail]) when is_binary(Bin) ->
    compile2(emit(Prog0, [Bin]), Tail);

%% Nothing else worked, look for the word in our dictionaries and base words,
%% maybe it is a literal, too
compile2(_Prog0 = #j1prog{}, [Word | _Tail]) ->
    ?COMPILE_ERROR("E4 J1C: Unexpected ~p", [Word]).
    %compile2(emit(Prog0, [Word]), Tail).

%%%-----------------------------------------------------------------------------

%% @doc Create a label with current PC, and push its id onto condition stack.
%% The value for the label will be updated once ELSE or THEN is reached
-spec begin_condition(j1prog()) -> {uint(), j1prog()}.
begin_condition(Prog0 = #j1prog{condstack=CondStack}) ->
    {F, Prog1} = create_label(Prog0),
    {F, Prog1#j1prog{condstack=[F | CondStack]}}.

%% @doc Creates a label with current PC and puts its id onto loop stack.
%% The label will be used for jump once UNTIL or AGAIN is reached
-spec begin_loop(j1prog()) -> {uint(), j1prog()}.
begin_loop(Prog0 = #j1prog{loopstack=LoopStack}) ->
    {F, Prog1} = create_label(Prog0),
    {F, Prog1#j1prog{loopstack=[F | LoopStack]}}.

-spec end_condition(j1prog()) -> {integer(), j1prog()}.
end_condition(#j1prog{condstack=[]}) ->
    ?COMPILE_ERROR("E4 J1C: ELSE or THEN have no matching IF");
end_condition(Prog0 = #j1prog{condstack=[CSTop | CondStack]}) ->
    {CSTop, Prog0#j1prog{condstack=CondStack}}.

-spec end_loop(j1prog()) -> {integer(), j1prog()}.
end_loop(#j1prog{loopstack=[]}) ->
    ?COMPILE_ERROR("E4 J1C: AGAIN or UNTIL have no matching BEGIN");
end_loop(Prog0 = #j1prog{loopstack=[LSTop | LoopStack]}) ->
    {LSTop, Prog0#j1prog{loopstack=LoopStack}}.

%% @doc Create a new label id to be placed in code (resolved at load time)
create_label(Prog0 = #j1prog{label_id = F0}) ->
    %Prog1 = emit(Prog0, #j1label{label = F0}),
    {F0, Prog0#j1prog{
        label_id = F0 + 1
        %labels = [{F0, PC} | Labels]
    }}.

%% @ doc Pop a label id from the cond stack and update #j1prog.labels with
%% the PC+Offset value.
cond_update_label(Prog0 = #j1prog{}, _Offset) ->
    {F, Prog1} = end_condition(Prog0),
    emit(Prog1, #j1label{label = F}).
%%    Prog1#j1prog{labels = orddict:store(F, PC + Offset, Labels)}.

prog_add_export(Prog0 = #j1prog{exports = Expt},
                Fun,
                Arity) ->
    {Prog1, _} = atom_index_or_create(Prog0, Fun),
    Expt1 = [{Fun, Arity} | Expt],
    Prog1#j1prog{exports = Expt1}.

as_int(I) when is_binary(I) -> binary_to_integer(I).

%% @doc Adds a word to the dictionary, records current program length (program
%% counter position) as the word address.
-spec prog_add_word(j1prog(), FA :: k_local() | binary()) -> j1prog().
prog_add_word(Prog0 = #j1prog{dict=Dict},
              #k_local{name=#k_atom{val=Fn}, arity=Arity}) ->
    {F, Prog1} = create_label(Prog0),
    Dict1 = orddict:store({Fn, as_int(Arity)}, F, Dict),
    {Prog2, _} = atom_index_or_create(Prog1, Fn),
    Prog2#j1prog{dict=Dict1};
prog_add_word(Prog0 = #j1prog{dict=Dict}, Word) when is_binary(Word) ->
    {F, Prog1} = create_label(Prog0),
    Dict1 = orddict:store(Word, F, Dict),
    {Prog2, _} = atom_index_or_create(Prog1, Word),
    Prog2#j1prog{dict=Dict1}.

%% @doc Adds a NIF name to the dictionary, does not register any other data
%% just marks the name as existing.
-spec prog_add_nif(j1prog(), Word :: binary(), Index :: neg_integer())
                  -> j1prog().
prog_add_nif(#j1prog{}, Word, Index) when Index >= 0 ->
    ?COMPILE_ERROR("E4 J1C: Bad NIF index ~p in :NIF directive ~p",
                   [Index, Word]);
prog_add_nif(Prog0 = #j1prog{dict_nif=Dict}, Word, Index) ->
    Dict1 = orddict:store(Word, Index, Dict),
    Prog0#j1prog{dict_nif=Dict1}.

emit(Prog0 = #j1prog{output=Out}, IOList) ->
    Prog0#j1prog{output=[IOList | Out]}.

%% @doc Looks up an atom in the atom table, returns its paired value or creates
%% a new atom, assigns it next available index and returns it
atom_index_or_create(Prog0 = #j1prog{atom_id=AtomId, atoms=Atoms}, Value)
    when is_binary(Value) ->
    case orddict:find(Value, Atoms) of
        error ->
            Prog1 = Prog0#j1prog{
                atom_id=AtomId + 1,
                atoms=orddict:store(Value, AtomId, Atoms)
            },
            {Prog1, AtomId};
        {ok, Existing} ->
            {Prog0, Existing}
    end.

%% @doc Looks up a literal in the literal table, returns its paired value or
%% creates a new literal, assigns it next available index and returns it
literal_index_or_create(Prog0 = #j1prog{lit_id=LitId, literals=Literals},
                        Value) ->
    case orddict:find(Value, Literals) of
        error ->
            Prog1 = Prog0#j1prog{
                lit_id=LitId + 1,
                literals=orddict:store(Value, LitId, Literals)
            },
            {Prog1, LitId};
        {ok, Existing} ->
            {Prog0, Existing}
    end.

emit_lit(Prog0 = #j1prog{}, atom, Word) when is_atom(Word) ->
    {Prog1, AIndex} = atom_index_or_create(Prog0, Word),
    emit(Prog1, #j1atom{id = AIndex, debug = Word});
%%emit_lit(Prog0 = #j1prog{}, integer, X) ->
%%    emit(Prog0, <<1:1, X:?J1_LITERAL_BITS/signed>>);
emit_lit(Prog0 = #j1prog{}, mfa, {M, F, A}) ->
    M1 = eval(M),
    F1 = eval(F),
    A1 = erlang:binary_to_integer(A),
    {Prog1, LIndex} = literal_index_or_create(Prog0, {'$MFA', M1, F1, A1}),
    emit(Prog1, #j1lit{id = LIndex, debug = {M, F, A}});
emit_lit(Prog0 = #j1prog{}, funarity, {F, A}) ->
    F1 = eval(F),
    A1 = erlang:binary_to_integer(A),
    {Prog1, LIndex} = literal_index_or_create(Prog0, {'$FA', F1, A1}),
    emit(Prog1, #j1lit{id = LIndex, debug = {F, A}});
emit_lit(Prog0 = #j1prog{}, arbitrary, Lit) ->
    {Prog1, LIndex} = literal_index_or_create(Prog0, Lit),
    emit(Prog1, #j1lit{id = LIndex, debug = Lit}).

eval(#k_atom{val=A}) -> erlang:binary_to_atom(A, utf8).
