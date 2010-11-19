Definitions.
A = [a-zA-Z]
D = [0-9]

Rules.
\"[^\"]+\" :
    {token, {string, TokenLine, string:strip(TokenChars, both, $")}}.
;[^\n]+ :
	skip_token.
:[^\s\n\t\(\)\"]+ :
    {token, {atom, TokenLine, list_to_atom(string:strip(TokenChars, left, $:))}}.
{D}+\.{D}+((E|e)(\+|\-)?{D}+)? :
    {token,{float, TokenLine, list_to_float(TokenChars)}}.
{D}+ : 
    {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
\( :
    {token, {'(', TokenLine}}.
\) :
    {token, {')', TokenLine}}.
[\s\n\t]+ :
    skip_token.
[^\s\n\t\(\)\"]+ :
    {token, {identifier, TokenLine, TokenChars}}.

Erlang code.
