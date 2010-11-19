-module(ghazal_eval).

-compile(export_all).


%%
%% construct AST from code
%%

ast(Code) ->
    {ok, Tokens, EndLine} = ghazal_lexer:string(Code),
    {ok, AST} = ghazal_parser:parse(Tokens ++  [{'$end', 1}]),
    ast_simplify(AST).

ast_simplify({string, _Line, S}) -> S;
ast_simplify({atom, _Line, A}) -> A;
ast_simplify({integer, _Line, I}) -> I;
ast_simplify({float, _Line, F}) -> F;
ast_simplify(Vals) when is_list(Vals) -> lists:map(fun ast_simplify/1, Vals);
ast_simplify(Val) -> Val.



builtins() ->
    dict:from_list(
      [
        {"+", {builtin, fun(X, Y) -> X + Y end}},
        {"-", {builtin, fun(X, Y) -> X - Y end}},
        {"*", {builtin, fun(X, Y) -> X * Y end}},
        {"/", {builtin, fun(X, Y) -> X / Y end}},
        {"println", 
            {builtin, fun(X) -> io:format("~p~n", [X]) end}
        }
      ]).

%%
%% eval() code
%%

eval(Code) ->
    AST = ast(Code),
    Globals = builtins(),
    Locals = dict:new(),
    eval(AST, Globals, Locals).

% handle defmodule
eval([{identifier, _Line, "defmodule"}|Rest], Globals, Locals) ->
    {_ModName, Globals1} = eval_defmodule(Rest, Globals, Locals),
    {none, Globals1, Locals};

% handle defun
eval([{identifier, _Line, "defun"}|Rest], Globals, Locals) ->
    {Val, Globals1} = eval_defun(Rest, Globals, Locals),
    {Val, Globals1, Locals};

% handle let
eval([{identifier, _Line, "let"}|Rest], Globals, Locals) ->
    {Val, Globals1} = eval_let(Rest, Globals, Locals),
    {Val, Globals1, Locals};

% handle function call, built-in call if possible
eval([{identifier, _Line, Fn}|Rest]=Call, Globals, Locals) ->
    case is_callable(Fn, Globals) of
        true -> 
            {Val, Globals1} = eval_call(Call, Globals, Locals),
            {Val, Globals1, Locals};
        false ->
            eval_list(Call, Globals, Locals)
    end;

% handle list of expressions (let-body, fun-body, etc) -- return val of last expression
eval(L, Globals, Locals) when is_list(L) ->
    eval_list(L, Globals, Locals);

% handle variable => lookup in Locals and Globals
eval({identifier, _Line, Var}, Globals, Locals) ->
    io:format("eval_variable: ~p in (Globals=~p & Locals=~p)~n", [Var, dict:to_list(Globals), dict:to_list(Locals)]),
    Val1 = case dict:find(Var, Locals) of
        {ok, Val} -> Val;
        error -> dict:find(Var, Globals)
    end,
    io:format("   variable return = ~p~n", [Val1]),
    {Val1, Globals, Locals};

% handle misc -- atoms, strings, numbers, etc
eval(Val, Globals, Locals) -> {Val, Globals, Locals}.


%%
%% Module defns
%%

eval_defmodule([{identifier, _Line, ModName} | Rest], Globals, Locals) ->
    {_Vals, Globals1, _} = eval(Rest, Globals, Locals),
    {ModName, Globals1}.

%%
%% Function defns
%%

eval_defun([{identifier, _Line, Name}|[ArgsList|FunBody]], Globals, Locals) ->
    Args = lists:map(fun (X) -> erlang:atom_to_list(X) end, ArgsList),
    Thunk = {thunk, Locals, Args, FunBody},
    Globals1 = dict:store(Name, Thunk, Globals), 
    {none, Globals1}.

%%
%% Function calls
%%

eval_call(Call, Globals, Locals) ->
    [{identifier, _Line, Name}|Args] = Call,
    io:format("eval_call:~n Name = ~p, Args = ~w~n", [Name, Args]),
    {ArgValues, Globals1} = eval_call_args(Args, Globals, Locals),
    eval_call_body(Name, ArgValues, Globals1).
    
eval_call_args(Args, Globals, Locals) ->
    {ArgV, Globals3} = 
        lists:foldl(
            fun (Arg, {Vals, Globals1}) ->
                {Val, Globals2, _} = eval(Arg, Globals1, Locals),
                {[Val|Vals], Globals2}
            end,
            {[], Globals},
            Args
        ),
    ArgValues = lists:reverse(ArgV),
    io:format(" eval_call_args: (after eval) = ~w~n", [ArgValues]),
    {ArgValues, Globals3}.

eval_call_body(Name, ArgValues, Globals) ->
    {ok, Target} = dict:find(Name, Globals),
    case Target of
        {thunk, Closure, ArgsList, FunBody} -> 
            Locals = ghazal_utils:dict_store_list(lists:zip(ArgsList, ArgValues), Closure),
            io:format(" Evaling FunBody = ~p with locals = ~p~n", [FunBody, Locals]),
            {Vals, Globals1, _} = eval(FunBody, Globals, Locals),
            Val = lists:last(Vals),
            io:format("Return = ~p~n", [Val]),
            {Val, Globals1};
        {builtin, Fun} ->
            Val = erlang:apply(Fun, ArgValues),
            io:format("Built-in return = ~p~n", [Val]),
            {Val, Globals}
    end.

is_callable(Name, Globals) ->
    case dict:find(Name, Globals) of
        {ok, {thunk, _, _, _}} -> true;
        {ok, {builtin, _}} -> true;
        _ -> false
    end.

%%
%% Let expressions
%%

eval_let([Bindings|LetBody], Globals, Locals) ->
    io:format("eval_let:~n"),
    {Globals1, Locals1} = eval_let_bindings(Bindings, Globals, Locals),
    io:format(" Evaling LetBody = ~p with locals = ~p~n", [LetBody, dict:to_list(Locals1)]),
    {Vals, Globals2, _} = eval(LetBody, Globals1, Locals1),
    Val = lists:last(Vals),
    io:format("  let return: ~w~n", [Val]),
    {Val, Globals2}.

eval_let_bindings([], Globals, Locals) ->
    {Globals, Locals};
eval_let_bindings([Var,Val|RemBindings], Globals, Locals) ->
    {Val1, Globals1, _Locals} = eval(Val, Globals, Locals),
    io:format("eval_let_bindings: (~p eval'd to ~p)~n", [Var, Val1]),
    Locals1 = dict:store(erlang:atom_to_list(Var), Val1, Locals),
    eval_let_bindings(RemBindings, Globals1, Locals1).

%%
%% list of expressions (bodies of code)
%%

eval_list(L, Globals, Locals) ->
    io:format("eval_list:~n ~p~n", [L]),
    {Vals3, Globals3, Locals3} = 
        lists:foldl(
            fun(Item, {Vals1, Globals1, Locals1}) ->
                {Val, Globals2, Locals2} = eval(Item, Globals1, Locals1),
                {[Val|Vals1], Globals2, Locals2}
            end,
            {[], Globals, Locals}, L
            ),
    io:format("  list returns: ~w~n", [lists:reverse(Vals3)]),
    {lists:reverse(Vals3), Globals3, Locals3}.
    