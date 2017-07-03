%%% @doc Statistics collection to give idea about value and opcode frequencies
%%% in the processed code.
%%% @end

-module(uasm_stats).

%% API
-export([
  count_opcode/1,
  count_value/1,
  dump/1,
  init/0
]).


init() ->
  %% Collects stats per instruction (before they are diversified by arg size)
  ets:new(instr_stat, [public, named_table]),

  %% Collects stats per value type
  ets:new(value_stat, [public, named_table]).


count_opcode(Identifier) when is_integer(Identifier) ->
  ets:update_counter(instr_stat, Identifier, 1, {Identifier, 0}).


count_value(Identifier) when is_atom(Identifier) ->
  ets:update_counter(value_stat, Identifier, 1, {Identifier, 0}).


dump(Filename) ->
  Dump = [
    "Instruction stats:\n",
    dump_tab(instr_stat),
    "\n"
    "Data types stats:\n",
    dump_tab(value_stat)
  ],
  file:write_file(Filename, Dump).


dump_tab(Tab) ->
  Pairs0 = ets:tab2list(Tab),
  Pairs1 = lists:keysort(2, Pairs0),
  lists:map(fun({K, V}) ->
              io_lib:format("~p\t~p\n", [K, V])
            end,
            Pairs1).
