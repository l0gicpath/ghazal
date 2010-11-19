-module(ghazal_leex).

-export([gen_lexer/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test/ghazal_lexer_test.erl").
-endif.

gen_lexer() ->
    leex:file("ghazal_lexer.xrl", [{scannerfile, "ghazal_lexer.erl"}]).
