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
  e4c:try_do("Save binary output",
             fun() ->
               IOList = e4asm_file:to_iolist(BC),
               file:write_file(e4asm_file:bin_filename(InputPath),
                               iolist_to_binary(IOList))
             end),
  ok.
