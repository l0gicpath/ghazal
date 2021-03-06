-module(ghazal_utils).

-export([dict_combine/1, dict_combine/2, dict_store_list/2, read_code/1]).

% combine D1 and D2 such that D2's keys overwrite any common keys
dict_combine(D1, D2) -> dict:merge(fun (_K, _V1, V2) -> V2 end, D1, D2).

% combine D1, D2, D3...and so on
dict_combine([]) -> dict:new();
dict_combine([D]) -> D;
dict_combine([D1,D2|Rest]) -> dict_combine([dict_combine(D1, D2)|Rest]).

% store a bunch of {K,V} pairs into a dict
dict_store_list(KVs, D) -> dict_combine(D, dict:from_list(KVs)).

% read from file to binary
read_code(FilePath) ->    
    case file:read_file(FilePath) of
        {ok, B} ->
            binary_to_list(B);
        {error, Reason} ->
            io:format("[Error]: could not open file ~p (~p)~n", [FilePath, Reason]),
            exit(1)
    end.