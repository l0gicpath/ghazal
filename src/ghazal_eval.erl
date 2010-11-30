-module(ghazal_eval).

-export([ast/1, eval/1, eval/3]).


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


%%
%% eval() code
%%

eval(Code) ->
    AST = ast(Code),
    Globals = ghazal_native:builtins(),
    Locals = dict:new(),
    eval(AST, Globals, Locals).

% handle defmodule
eval([{identifier, _Line, "defmodule"}|Rest], Globals, Locals) ->
    {_ModName, Globals1} = eval_defmodule(Rest, Globals, Locals),
    {none, Globals1, Locals};

eval([{identifier, _Line, "include"}|Rest], Globals, Locals) ->
    {Val, Globals1} = eval_include(Rest, Globals, Locals),
    {Val, Globals1, Locals};

% handle defun
eval([{identifier, _Line, "defun"}|Rest], Globals, Locals) ->
    {Val, Globals1} = eval_defun(Rest, Globals, Locals),
    {Val, Globals1, Locals};

% handle lambda defn
eval([{identifier, _Line, "lambda"}|Rest], Globals, Locals) ->
    Val = eval_lambda(Rest, Globals, Locals),
    {Val, Globals, Locals};

% handle let
eval([{identifier, _Line, "let"}|Rest], Globals, Locals) ->
    {Val, Globals1} = eval_let(Rest, Globals, Locals),
    {Val, Globals1, Locals};

% handle if
eval([{identifier, _Line, "if"}|Rest], Globals, Locals) ->
    {Val, Globals1} = eval_if(Rest, Globals, Locals),
    {Val, Globals1, Locals};

% handle function call, built-in call if possible
eval([{identifier, _Line, Name}|Rest]=Call, Globals, Locals) ->
    case get_callable(Name, Globals, Locals) of
        error ->
            eval_list(Call, Globals, Locals);
        Target -> 
            {Val, Globals1} = eval_call(Call, Target, Globals, Locals),
            {Val, Globals1, Locals}
    end;

% handle list of expressions (let-body, fun-body, etc) -- returns list of vals
eval(L, Globals, Locals) when is_list(L) ->
    eval_list(L, Globals, Locals);

% handle variable => lookup in Locals and Globals
eval({identifier, _Line, Var}, Globals, Locals) ->
    io:format("eval_variable: ~p in (Globals ~n Locals=~p)~n", [Var, dict:to_list(Locals)]),
    {ok, Val1} = resolve_identifier(Globals, Locals, Var),
    io:format("   variable return = ~p~n", [Val1]),
    {Val1, Globals, Locals};

% handle misc -- atoms, strings, numbers, etc
eval(Val, Globals, Locals) -> {Val, Globals, Locals}.

resolve_identifier(Globals, Locals, Name) ->
    case dict:find(Name, Locals) of
        {ok, Val} -> {ok, Val};
        error -> ghazal_ns:resolve(Globals, Name)
    end.

%%
%% Module defns
%%

eval_defmodule([{identifier, _Line, ModName} | Rest], Globals, Locals) ->
    CurrentNS = ghazal_ns:current(Globals),
    
    % update Globals with the new namespace
    NewNS = ghazal_ns:join(CurrentNS, ModName),
    Globals1 = ghazal_ns:set_current(Globals, NewNS),
    
    % eval everything under the new namespace
    {_Vals, Globals2, _} = eval(Rest, Globals1, Locals),
    
    % restore the old namespace back.
    Globals3 = ghazal_ns:set_current(Globals2, CurrentNS),
    {ModName, Globals3}.

eval_include(IncludePaths, Globals, Locals) ->
    Globals3 = lists:foldl(
        fun(FilePath, Globals1) ->
            % check if already loaded
            case ghazal_ns:resolve(Globals1, FilePath) of
                {ok, _Val} -> Globals1;
                error -> 
                    Code = ghazal_utils:read_code(FilePath),
                    AST = ast(Code),
                    {_, Globals2, _Locals} = eval(AST, Globals1, dict:new()),
            
                    % mark as loaded in globals for future reference
                    Globals2
            end
        end,
        Globals,
        IncludePaths
    ),
    {none, Globals3}.

%%
%% Function defns
%%

eval_defun([{identifier, _Line, Name}|[ArgsList|FunBody]], Globals, Locals) ->
    Args = lists:map(fun (X) -> erlang:atom_to_list(X) end, ArgsList),
    Thunk = {thunk, ghazal_ns:current(Globals), Locals, Args, FunBody},
    DefunName = ghazal_ns:join(ghazal_ns:current(Globals), Name),
    Globals1 = dict:store(DefunName, Thunk, Globals), 
    {none, Globals1}.

%%
%% Lambda defns
%%

eval_lambda([ArgsList|Body], Globals, Locals) ->
    Args = lists:map(fun (X) -> erlang:atom_to_list(X) end, ArgsList),
    Thunk = {thunk, ghazal_ns:current(Globals), Locals, Args, Body},
    Thunk.


%%
%% Function calls
%%

eval_call(Call, Target, Globals, Locals) ->
    [{identifier, _Line, Name}|Args] = Call,
    io:format("eval_call:~n Name = ~p, Args = ~w~n", [Name, Args]),
    {ArgValues, Globals1} = eval_call_args(Args, Globals, Locals),
    eval_call_body(Target, ArgValues, Globals1).
    
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

eval_call_body(Target, ArgValues, Globals) ->
    case Target of
        {thunk, NS, Closure, ArgsList, FunBody} -> 
            Locals = ghazal_utils:dict_store_list(lists:zip(ArgsList, ArgValues), Closure),
            io:format(" Evaling FunBody = ~p ~n with locals = ~p~n", [FunBody, Locals]),
            
            % restore lexical namespace before evaling Body
            Globals1 = ghazal_ns:set_current(Globals, NS),
            {Vals, Globals2, _} = eval(FunBody, Globals1, Locals),

            % revert back to original namespace
            Globals3 = ghazal_ns:set_current(Globals2, ghazal_ns:current(Globals)),

            % return result
            Val = lists:last(Vals),
            io:format("Return = ~p~n", [Val]),    
            {Val, Globals3};
        {builtin, Fun} ->
            Val = erlang:apply(Fun, ArgValues),
            io:format("Built-in return = ~p~n", [Val]),
            {Val, Globals}
    end.

get_callable(Name, Globals, Locals) ->
    case resolve_identifier(Globals, Locals, Name) of
        {ok, {thunk, _, _, _, _}=Thunk} -> Thunk;
        {ok, {builtin, _}=Builtin} -> Builtin;
        _ -> error
    end.

%%
%% Let expressions
%%

eval_let([Bindings|LetBody], Globals, Locals) ->
    io:format("eval_let:~n"),
    {Globals1, Locals1} = eval_let_bindings(Bindings, Globals, Locals),
    io:format(" Evaling LetBody = ~p ~n with locals = ~p~n", [LetBody, dict:to_list(Locals1)]),
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
%% if expressions
%%

eval_if([TestBody, ThenBody, ElseBody], Globals, Locals) ->
    io:format("eval_if:~n"),
    {TestResult, Globals1, _} = eval(TestBody, Globals, Locals),
    io:format("  test result: ~p~n", [TestResult]),
    {Vals1, Globals2, _} =
    case TestResult of
        true -> eval([ThenBody], Globals1, Locals);
        false -> eval([ElseBody], Globals1, Locals)
    end,
    Val = lists:last(Vals1),
    io:format("  if return: ~w~n", [Val]),
    {Val, Globals2}.

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
    