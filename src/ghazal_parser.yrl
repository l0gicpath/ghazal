Nonterminals list elements element.
Terminals '(' ')' integer float atom string identifier.
Rootsymbol list.

list -> '(' ')'.
list -> '(' elements ')' : '$2'.
elements -> element elements : ['$1'|'$2'].
elements -> '$empty' : [].
element -> integer : '$1'.
element -> float : '$1'.
element -> atom : '$1'.
element -> string : '$1'.
element -> list : '$1'.
element -> identifier : '$1'.
