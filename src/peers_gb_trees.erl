
-module(peers_gb_trees).

-export([empty/0, size/1, list/1, lookup/2, enter/3, delete/2]).


empty() -> gb_trees:empty().

size(Tree) -> gb_trees:size(Tree).

list(Tree) -> gb_trees:to_list(Tree).

lookup(Tree, Key) ->
    case gb_trees:lookup(Key, Tree) of
    none -> false;
    {value, Value} -> {value, Value}
    end.

enter(Tree, Key, Value) -> gb_trees:enter(Key, Value, Tree).

delete(Tree, Key) -> gb_trees:delete_any(Key, Tree).

