Nonterminals program lists list elements element.
Terminals '(' ')' integer float atom string identifier.
Rootsymbol program.

program -> lists : '$1'.
lists -> list lists : ['$1'|'$2'].
lists -> '$empty': [].
list -> '(' elements ')' : '$2'.
elements -> element elements : ['$1'|'$2'].
elements -> '$empty' : [].
element -> integer : '$1'.
element -> float : '$1'.
element -> atom : '$1'.
element -> string : '$1'.
element -> list : '$1'.
element -> identifier : '$1'.
