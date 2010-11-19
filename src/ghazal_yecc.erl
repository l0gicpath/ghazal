-module(ghazal_yecc).

-export([gen_parser/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test/ghazal_parser_test.erl").
-endif.

gen_parser() ->
    yecc:file("ghazal_parser.yrl", [{parserfile, "ghazal_parser.erl"}]).
