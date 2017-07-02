%%% @doc Statistics collection to give idea about value and opcode frequencies
%%% in the processed code.
%%% @end
-module(e4asm_stats).

%% API
-export([init/0, count_opcode/1]).


init() ->
  %% Collects stats per instruction (before they are diversified by arg size)
  ets:new(instr_stat, [public, named_table]),

  %% Collects stats per value type
  ets:new(value_type_stat, [public, named_table]).


count_opcode(Identifier) when is_atom(Identifier) ->
  ets:update_counter(instr_stat, Identifier, 1, {Identifier, 0}).
