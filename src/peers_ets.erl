% unstested code, needs to be benchmarked, to be usefully one probably need
% to increse ERL_MAX_ETS_TABLES

-module(peers_ets).

-export([empty/0, size/1, list/1, lookup/2, enter/3, delete/2]).


%empty() -> ets:new(peers, [private]).
empty() -> ets:new(peers, [public]).

size(Tab) -> ets:info(Tab, size).

list(Tab) ->
    [X || [X] <- ets:match(Tab, '$1')].

lookup(Tab, Key) ->
    case ets:lookup(Tab, Key) of
    [] -> false;
    [{Key, Value}] -> {value, Value}
    end.

enter(Tab, Key, Value) ->
    ets:insert(Tab, {Key, Value}).

delete(Tab, Key) ->
    ets:delete(Tab, Key).

