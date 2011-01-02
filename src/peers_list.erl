
-module(peers_list).

-export([empty/0, size/1, list/1, lookup/2, enter/3, delete/2]).


empty() -> [].

size(List) -> length(List).

list(List) -> List.

lookup(List, Key) ->
    case lists:keysearch(Key, 1, List) of
    false -> false;
    {value, {Key, Value}} -> {value, Value}
    end.

enter(List, Key, Value) ->
    [{Key, Value}|lists:keydelete(Key, 1, List)].

delete(List, Key) ->
    lists:keydelete(Key, 1, List).


