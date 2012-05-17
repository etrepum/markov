-module(markov_gb).
-export([new/1, from_list/2, to_list/1, append/3, lookup/2,
         choose_nth/2]).

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
            gb_trees:insert(K, {1, increment(V, gb_trees:empty())}, T);
        {value, {Count, SubT}} ->
            gb_trees:update(K, {1 + Count, increment(V, SubT)}, T)
    end.

increment(K, SubT) ->
    case gb_trees:lookup(K, SubT) of
        none ->
            gb_trees:insert(K, 1, SubT);
        {value, N} ->
            gb_trees:update(K, 1 + N, SubT)
    end.

lookup(K, T) ->
    gb_trees:lookup(K, T).


choose_nth(N, SubT) ->
    choose_nth_iter(N, gb_trees:next(gb_trees:iterator(SubT))).

choose_nth_iter(N, {_K, V, Iter}) when N > V ->
    choose_nth_iter(N - V, gb_trees:next(Iter));
choose_nth_iter(_N, {K, _V, _Iter}) ->
    K.

subtree_from_list(L) ->
    lists:foldl(fun ({K, V}, {C, T}) ->
                        {V + C, gb_trees:insert(K, V, T)}
                end,
                {0, gb_trees:empty()},
                L).

to_orddict_iter(none) ->
    [];
to_orddict_iter({K, V, Iter}) ->
    [{K, to_orddict_subtree(V)} | to_orddict_iter(gb_trees:next(Iter))].

to_orddict_subtree({_Count, T}) ->
    gb_trees:to_list(T).
