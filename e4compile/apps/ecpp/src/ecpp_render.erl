%% @doc ecpp source render API. Takes a tree produced by ecpp_ast and renders
%% its text representation.
%% @end

-module(ecpp_render).

-export([write/2]).

-include_lib("ecpp/include/ecpp_ast.hrl").

write(F, AST) ->
    IO = lists:flatten(structured_code(AST, 0)),
    Filename = filename(F),
    io:format("Writing output to ~s~n", [Filename]),
    e4c:debug_write_term("dump.txt", iolist_to_binary(IO)),
    file:write_file(Filename, IO).

filename(F) ->
    RDot = string:rchr(F, $.),
    string:sub_string(F, 1, RDot) ++ "cpp".

%% @doc Process top level code structure, where each element is an operator
%% block or an operator, prefixed with indentation spaces and ';' after it.
structured_code(B, _Indent) when is_binary(B) ->
    B;
structured_code(L, Indent) when is_list(L) ->
    lists:map(fun(Elem) -> structured_code(Elem, Indent) end, L);
structured_code(#cpp_module{name = N, body = #cpp_block{code = Body}}, Indent) ->
    [fmt("// Module ~s~n", [N]),
     structured_code(Body, Indent)
    ];
structured_code(#cpp_block{before = [], aftr = [], code = []}, _Indent) -> [];
structured_code(#cpp_block{before = Bef, aftr = Aft, code = Code}, Indent) ->
    [indent(Indent), "{\n",
     structured_code(Bef, Indent + 1),
     structured_code(Code, Indent + 1),
     structured_code(Aft, Indent + 1),
     indent(Indent), "}\n"
    ];
structured_code(#cpp_if{condition = Cond, true = True, false = False}, Indent) ->
    [indent(Indent), "if (", expr(Cond), ") {\n",
     structured_code(True, Indent + 1),
     indent(Indent),
     case False of
         [] -> "}\n";
         _ -> ["} else {\n", structured_code(False, Indent + 1), "}\n"]
     end
    ];
structured_code(#cpp_fun{type = T, name = Name, args = Args, code = Code}, Indent) ->
    CppArgs = lists:map(fun(A) -> ["Term ", expr(A)] end, Args),
    [structured_code(T, Indent), " ",
     Name, "(", join(CppArgs, ", "), ")\n",
     structured_code(Code, Indent),
     "\n"
    ];
structured_code(#cpp_comment{comment = Co}, Indent) ->
    [ indent(Indent),
      "// ", Co, "\n"
    ];
structured_code(#cpp_return{val = Val}, Indent) ->
    [indent(Indent), "return ", expr(Val), ";\n"];
structured_code(#cpp_assign{lhs = Lhs, rhs = Rhs}, Indent) ->
    [indent(Indent),
     "auto ", expr(Lhs), " = ", expr(Rhs), ";\n"
    ];
structured_code(#cpp_try{}, Indent) ->
    [ indent(Indent), "try\n"
    ];
structured_code(#cpp_catch{clauses = Clauses}, Indent) ->
    [indent(Indent), "catch {\n",
     structured_code(Clauses, Indent + 1),
     indent(Indent), "}\n"
    ];
structured_code(#cpp_call{} = C, Indent) ->
    [indent(Indent), expr(C), ";\n"].

%%%-----------------------------------------------------------------------------

expr(#cpp_block{} = B) -> structured_code(B, 0);
expr(Bin) when is_binary(Bin) -> Bin;
expr(#cpp_ref{ref = Ref}) ->
    ["& ", expr(Ref)];
expr(#cpp_lit{val = Lit}) ->
    expr(erlang_literal(Lit));
expr(#cpp_var{name = N}) ->
    N;
expr(#cpp_call{target = Target, args = Args, tailcall = _TC}) ->
    CallArgs = lists:map(fun(A) -> expr(A) end, Args),
    [structured_code(Target, 0),
     "(",
     join(CallArgs, ", "),
     ")"
    ].

%%%-----------------------------------------------------------------------------

indent(Indent) ->
    lists:duplicate(Indent * 2, $ ).

%%as_binary(B) when is_binary(B) -> B;
%%as_binary(X) when is_integer(X) -> erlang:integer_to_binary(X);
%%as_binary(S) when is_list(S) -> erlang:list_to_binary(S).

join([], _Sep) -> [];
join([H | T], Sep) ->
    [H, [[Sep, Elem] || Elem <- T]].

fmt(Format, Args) ->
    erlang:iolist_to_binary(io_lib:format(Format, Args)).

erlang_literal(X) when is_atom(X) ->
    ecpp_ast:call("make_atom", [ecpp_ast:string(X)]);
erlang_literal(X) when is_integer(X) ->
    integer_to_binary(X);
erlang_literal(T) when is_tuple(T) ->
    ecpp_ast:call("make_tuple",
                  lists:map(fun erlang_literal/1,
                            erlang:tuple_to_list(T)));
erlang_literal([]) ->
    ecpp_ast:call("make_nil", []).
