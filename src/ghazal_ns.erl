-module(ghazal_ns).

-export([current/1, set_current/2, join/2, join/1, resolve/2]).

-define(ROOT_NS, "").

current(Globals) ->
	case dict:find("?MODULE", Globals) of
		{ok, NS} -> NS;
		error -> ?ROOT_NS
	end.

set_current(Globals, NS) -> dict:store("?MODULE", NS, Globals).

join(NS1, NS2) -> NS1 ++ "." ++ NS2.
join(Namespaces) when is_list(Namespaces) -> string:join(Namespaces, ".").

resolve(Globals, Name) ->
    case dict:find(join(?ROOT_NS, Name), Globals) of
        {ok, Val} -> 
            {ok, Val};
        error -> 
            FullName = join(current(Globals), Name),
            dict:find(FullName, Globals)
    end.
