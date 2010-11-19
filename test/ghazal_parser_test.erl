simple_test() ->
    {ok, AST} = ghazal_parser:parse([{'(',1}, {atom, 1, 'peter'}, {string, 2, "foobar"}, {')', 1}, {'$end', 1}]),
    [{atom, 1, 'peter'}, {string, 2, "foobar"}] = AST.
