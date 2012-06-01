-module(markov_cstack).
-export([new/1, from_list/2, to_list/1, append/3, lookup/2,
         choose_nth/2]).

-define(SUB, cstack).
-define(WORD_MAX, 100).

new([]) ->
    gb_trees:empty().

from_list(L, EmptyT) ->
    lists:foldl(fun ({K, V}, T) ->
                        gb_trees:insert(K, subtree_from_list(V), T)
                end,
                EmptyT,
                L).

to_list(T) ->
    to_orddict_iter(gb_trees:next(gb_trees:iterator(T))).

append(K, V, T) ->
    case gb_trees:lookup(K, T) of
        none ->
            gb_trees:insert(K, increment(V, ?SUB:new()), T);
        {value, SubT} ->
            gb_trees:update(K, increment(V, SubT), T)
    end.

increment(K, SubT) ->
    ?SUB:increment(K, 1, SubT).

lookup(K, T) ->
    case gb_trees:lookup(K, T) of
        {value, SubT} ->
            case ?SUB:total(SubT) of
                Total when Total > 0 ->
                    {value, {Total, SubT}};
                Total ->
                    error({total_is_zero, Total, K, SubT})
            end;
        none ->
            none
    end.

choose_nth(N, SubT) ->
    ?SUB:seek(N - 1, SubT).

subtree_from_list(L) ->
    ?SUB:from_list(L).

to_orddict_iter(none) ->
    [];
to_orddict_iter({K, V, Iter}) ->
    [{K, to_orddict_subtree(V)} | to_orddict_iter(gb_trees:next(Iter))].

to_orddict_subtree(T) ->
    ?SUB:to_list(T).
