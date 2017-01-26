-ifndef(E4_HRL).
-define(E4_HRL, 1).

-define(COMPILE_ERROR(Message),
    begin
        io:format("~s~n", [color:red(Message)]),
        erlang:error(compile_error)
    end).

-define(COMPILE_ERROR(Format, Args),
    begin
        io:format(Format, Args),
        erlang:error(compile_error)
    end).

-define(COLOR_TERM(Color, T), color:Color(io_lib:format("~n~p", [T]))).

-endif. % E4_HRL
