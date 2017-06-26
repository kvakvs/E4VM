-module(e4asm_compile).
-export([process/2]).

%% @doc Takes: A module map #{} with postprocessed BEAM assembly with new
%%      E4 ASM instructions in it
%% Result: Writes binary e4b file with bytecode.
-spec process(string(), #{}) -> ok.
process(InputPath, Input) ->
  BC = e4c:try_do(
    "e4asm_pass_asm - Assembly to binary",
    fun() -> e4asm_pass_asm:compile(Input) end
  ),
  OutputPath = e4asm_file:bin_filename(InputPath),
  save_output(text, BC, OutputPath).


%% @doc Format the resulting bytecode as text (for debug and simplicity) or
%% as a binary (for final deployment).
save_output(Format, BC, OutputPath) ->
  e4c:try_do("Save " + erlang:atom_to_list(Format) + " output",
             fun() ->
               IOList = e4asm_file:to_iolist(Format, BC),
               file:write_file(OutputPath, iolist_to_binary(IOList))
             end),
  ok.
