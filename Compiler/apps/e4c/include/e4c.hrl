-ifndef(E4_HRL).
-define(E4_HRL, 1).

-define(COMPILE_ERROR(Message),
    begin
        e4c:error(header),
        io:format("~s~n", [Message]),
        e4c:error(footer),
        erlang:error(compile_error)
    end).

-define(COMPILE_ERROR(Format, Args),
    begin
        e4c:error(header),
        io:format(Format, Args),
        e4c:error(footer),
        erlang:error(compile_error)
    end).

-define(COLOR_TERM(Color, T), color:Color(io_lib:format("~p", [T]))).

-define(COMPILE_ERROR1(Message, Term1),
    begin
        e4c:error(header),
        io:format("~s: ~s", [color:whiteb(Message), ?COLOR_TERM(red, Term1)]),
        e4c:error(footer),
        erlang:error(compile_error)
    end).

-define(ASSERT(Cond, Msg), case (Cond) of
                               true -> ok;
                               _ -> ?COMPILE_ERROR(Msg)
                           end).

-endif. % E4_HRL
