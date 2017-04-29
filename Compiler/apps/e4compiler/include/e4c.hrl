-ifndef(E4C_HRL__).
-define(E4C_HRL__, 1).

-define(COMPILE_ERROR(Message),
  begin
    e4c:error(header),
    io:format(?MODULE_STRING ++ ": ~s~n", [Message]),
    e4c:error(footer),
    erlang:error(compile_error)
  end).

-define(COMPILE_ERROR(Format, Args),
  begin
    e4c:error(header),
    io:format(?MODULE_STRING ++ ": " ++ Format, Args),
    e4c:error(footer),
    erlang:error(compile_error)
  end).

-define(COLOR_TERM(Color, T), color:Color(io_lib:format("~p", [T]))).

-define(COMPILE_ERROR1(Message, Term1),
  begin
    e4c:error(header),
    io:format(?MODULE_STRING ++ ": ~s: ~s",
              [color:whiteb(Message), ?COLOR_TERM(red, Term1)]),
    e4c:error(footer),
    erlang:error(compile_error)
  end).

-define(ASSERT(Cond, Msg), case (Cond) of
                             true -> ok;
                             _ -> ?COMPILE_ERROR(Msg)
                           end).

-endif. % E4C_HRL
