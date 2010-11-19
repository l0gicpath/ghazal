simple_test() ->
    {ok, Tokens, 2} =
        ghazal_lexer:string(
          "(set square\n"
          "   (lambda (x) (* x x)))"),
    [{'(',1},
     {identifier,1,"set"},
     {identifier,1,"square"},
     {'(',2},
     {identifier,2,"lambda"},
     {'(',2},
     {identifier,2,"x"},
     {')',2},
     {'(',2},
     {identifier,2,"*"},
     {identifier,2,"x"},
     {identifier,2,"x"},
     {')',2},
     {')',2},
     {')',2}] = Tokens.
