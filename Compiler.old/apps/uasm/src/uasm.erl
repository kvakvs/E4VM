-module(uasm).
-export([process/2]).

%% @doc Takes: A module map #{} with postprocessed BEAM assembly with new
%%      MicroErlang ASM instructions in it
%% Result: Writes binary `.uerl` file with bytecode and optional `.stats.txt`
%%      file with frequency stats collected by `uasm_stats` module.
-spec process(string(), #{}) -> ok.
process(InputPath, Input) ->
  uasm_stats:init(),  % frequency stats are collected here
  BC = uerlc:try_do(
    "uasm_pass_asm - Assembly to binary",
    fun() -> uasm_pass_asm:compile(Input) end
  ),
  OutputPath = uasm_file:make_filename(InputPath, "uerl"),
  save_output(text, BC, OutputPath),
  StatsOutputPath = uasm_file:make_filename(InputPath, "stats.txt"),
  uasm_stats:dump(StatsOutputPath).


%% @doc Format the resulting bytecode as text (for debug and simplicity) or
%% as a binary (for final deployment).
save_output(Format, BC, OutputPath) ->
  uerlc:try_do("Save " ++ erlang:atom_to_list(Format) ++ " output",
               fun() ->
                 IOList = uasm_file:to_iolist(Format, BC),
               file:write_file(OutputPath, iolist_to_binary(IOList))
             end),
  ok.
