-module(e4_compiler).
-export([process/1]).

-include("e4_cf.hrl").
-import(e4_encode, [varint/1, val_zreg/1, val_int/1]).
-import(e4_pass_beam_forth, [asm_move/2, asm_syscall/1, f_emit_call/3, asm_apply/2]).

%% @doc Takes filename as input, produces compiled BEAM AST and processes it
process(F) ->
    case compile:file(F, [to_core, binary, report]) of
        {ok, _M, CoreErlang} ->
            CoreForth = e4_c2cf:process(CoreErlang),
            Forth = e4_cf2f:process(CoreForth),
            _Binary = e4_f2bin:process(Forth);
        E ->
            io:format("~n~s: ~p~n", [F, E])
    end.
