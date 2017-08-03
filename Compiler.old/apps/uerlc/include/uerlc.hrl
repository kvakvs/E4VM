-ifndef(UERLC_HRL__).
-define(UERLC_HRL__, 1).


%% When modifying regy and regx (maxarity) limits, please also modify bits in
%% uasm_encode_int where these macros are used
-define(LIMIT_MAX_REGY, 256).
-define(LIMIT_MAXARITY, 256).

-define(COMPILE_ERROR(Message),
  begin
    uerlc:error(header),
    io:format(?MODULE_STRING ++ ": ~s~n", [Message]),
    uerlc:error(footer),
    erlang:error(compile_error)
  end).

-define(COMPILE_ERROR(Format, Args),
  begin
    uerlc:error(header),
    io:format(?MODULE_STRING ++ ": " ++ Format, Args),
    uerlc:error(footer),
    erlang:error(compile_error)
  end).

-define(COLOR_TERM(Color, T), color:Color(io_lib:format("~p", [T]))).

-define(COMPILE_ERROR1(Message, Term1),
  begin
    uerlc:error(header),
    io:format(?MODULE_STRING ++ ": ~s: ~s",
              [color:whiteb(Message), ?COLOR_TERM(red, Term1)]),
    uerlc:error(footer),
    erlang:error(compile_error)
  end).

-define(ASSERT(Cond, Msg), case (Cond) of
                             true -> ok;
                             _ -> ?COMPILE_ERROR(Msg)
                           end).

-endif. % UERLC_HRL
