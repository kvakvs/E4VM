
-module(uerlc_pass_from_S).

%% API
-export([process/1]).

-include_lib("uerlc/include/uerlc.hrl").
-include_lib("uasm/include/uasm.hrl").

process({Module, _Exports, Attrs, Forms, _}) ->
  Mod0 = #{
    '$' => module,
    mod => Module,
    funs => orddict:new(),
    exports => orddict:new(),
    attrs => Attrs
  },

  process_forms(Forms, Mod0).

%%%-----------------------------------------------------------------------------

process_forms([], Mod) -> Mod;
process_forms([{function, Name, Arity, _, Code0} | Forms],
              Mod0 = #{'$' := module, funs := Funs0}) ->
  #{'$' := pfun, code := Code1, mod := Mod1} = process_fun(Code0, Mod0, []),

  Fun = #{
    '$' => e4fun,
    name => Name,
    arity => Arity,
    code => Code1
  },
  Funs1 = orddict:store({Name, Arity}, Fun, Funs0),
  process_forms(Forms, Mod1#{funs => Funs1});

process_forms([X | _Forms], _Mod) ->
  ?COMPILE_ERROR("Unknown form ~p", [X]).


process_fun([], Mod0, Out) ->
  #{'$' => pfun,
    code => lists:reverse(Out),
    mod => Mod0};

process_fun([{gc_bif, Name, Fail, Heap, Args, Result} | Tail], Mod0, Out) ->
  Cmd = #{
    '$' => e4bif,
    args => Args,
    fail => Fail,
    gc => Heap,
    name => Name,
    result => Result
  },
  process_fun(Tail, Mod0, [Cmd | Out]);

process_fun([{bif, Name, Fail, Args, Result} | Tail], Mod0, Out) ->
  Cmd = #{
    '$' => e4bif,
    args => Args,
    fail => Fail,
    name => Name,
    result => Result
  },
  process_fun(Tail, Mod0, [Cmd | Out]);

process_fun([{test, F, Fail, Args} | Tail], Mod0, Out) ->
  Cmd = #{
    '$' => e4bif,
    args => Args,
    fail => Fail,
    name => F
  },
  process_fun(Tail, Mod0, [Cmd | Out]);

process_fun([{call, Arity, Label} | Tail], Mod0, Out) ->
  Cmd = #{
    '$' => e4call,
    arity => Arity,
    target => Label
  },
  process_fun(Tail, Mod0, [Cmd | Out]);

process_fun([{call_last, Arity, Label, Dealloc} | Tail], Mod0, Out) ->
  Cmd = #{
    '$' => e4call,
    arity => Arity,
    dealloc => Dealloc,
    tailcall => true,
    target => Label
  },
  process_fun(Tail, Mod0, [Cmd | Out]);

process_fun([{call_only, Arity, Label} | Tail], Mod0, Out) ->
  Cmd = #{
    '$' => e4call,
    arity => Arity,
    tailcall => true,
    target => Label
  },
  process_fun(Tail, Mod0, [Cmd | Out]);

process_fun([{call_ext, Arity, {extfunc, M, F, Arity}} | Tail], Mod0, Out) ->
  Cmd = #{
    '$' => e4call,
    arity => Arity,
    target => {extfunc, M, F, Arity}
  },
  process_fun(Tail, Mod0, [Cmd | Out]);

process_fun([{call_ext_only, Arity, {extfunc, M, F, Arity}} | Tail],
            Mod0, Out) ->
  Cmd = #{
    '$' => e4call,
    arity => Arity,
    tailcall => true,
    target => {extfunc, M, F, Arity}
  },
  process_fun(Tail, Mod0, [Cmd | Out]);

process_fun([{call_ext_last, Arity, {extfunc, M, F, Arity}, De} | Tail],
            Mod0, Out) ->
  Cmd = #{
    '$' => e4call,
    arity => Arity,
    dealloc => De,
    tailcall => true,
    target => {extfunc, M, F, Arity}
  },
  process_fun(Tail, Mod0, [Cmd | Out]);

process_fun([{deallocate, N}, return | Tail], Mod0, Out) ->
  Cmd = #{
    '$' => e4ret,
    dealloc => N
  },
  process_fun(Tail, Mod0, [Cmd | Out]);

process_fun([return | Tail], Mod0, Out) ->
  Cmd = #{
    '$' => e4ret,
    dealloc => 0
  },
  process_fun(Tail, Mod0, [Cmd | Out]);

process_fun([{line, _} | Tail], Mod0, Out) ->
  %% TODO: maybe debug info comes from here
  process_fun(Tail, Mod0, Out);

%%process_fun([FunInfo = {func_info, {atom, _ModName}, {atom, Fun}, Arity},
%%             Lbl = {label, Label} | Tail], Mod0, Out) ->
%%  Mod1 = mark_function_start(Fun, Arity, Label, Mod0),
%%  process_fun(Tail, Mod1, [FunInfo, Lbl | Out]);

process_fun([Code | Tail], Mod0, Out) ->
  process_fun(Tail, Mod0, [Code | Out]).


%%mark_function_start(Fun, Arity, Label,
%%                    Mod0 = #{'$' := module, exports := Exports}) ->
%%  Exports1 = orddict:store({Fun, Arity}, Label, Exports),
%%  Mod0#{exports => Exports1}.