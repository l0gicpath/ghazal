-module(ghazal_native).

-export([builtins/0]).


builtins() ->
    dict:from_list(
      [
        % basic arithmetic operators
        {".+", {builtin, fun(X, Y) -> X + Y end}},
        {".-", {builtin, fun(X, Y) -> X - Y end}},
        {".*", {builtin, fun(X, Y) -> X * Y end}},
        {"./", {builtin, fun(X, Y) -> X / Y end}},
        {".%", {builtin, fun(X, Y) -> X rem Y end}},
                
        % comparison operators
        {".<", {builtin, fun(X, Y) -> X < Y end}},
        {".>", {builtin, fun(X, Y) -> X > Y end}},
        {".>=", {builtin, fun(X, Y) -> X >= Y end}},
        {".<=", {builtin, fun(X, Y) -> X =< Y end}},
        {".==", {builtin, fun(X, Y) -> X =:= Y end}},
        {".!=", {builtin, fun(X, Y) -> X =/= Y end}},
        
        % misc
        {".println", {builtin, fun(X) -> io:format("~p~n", [X]) end}}
      ]).