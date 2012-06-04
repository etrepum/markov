-module(markov_cstack).
-export([new/1, from_list/2, to_list/1, append/3, lookup/2,
         choose_nth/2]).

-define(DEFAULT_SUB, cstack).
-define(WORD_MAX, 100).

new([{module, M}]) ->
    {M, gb_trees:empty()};
new([]) ->
    new([{module, ?DEFAULT_SUB}]).

from_list(L, {M, EmptyT}) ->
    {M, lists:foldl(fun ({K, V}, T) ->
                            gb_trees:insert(K, M:from_list(V), T)
                    end,
                    EmptyT,
                    L)}.

to_list({M, T}) ->
    to_orddict_iter(M, gb_trees:next(gb_trees:iterator(T))).

append(K, V, {M, T}) ->
    {M, case gb_trees:lookup(K, T) of
            none ->
                gb_trees:insert(K, M:increment(V, 1, M:new()), T);
            {value, SubT} ->
                gb_trees:update(K, M:increment(V, 1, SubT), T)
        end}.

lookup(K, {M, T}) ->
    case gb_trees:lookup(K, T) of
        {value, SubT} ->
            case M:total(SubT) of
                Total when Total > 0 ->
                    {value, {Total, {M, SubT}}};
                Total ->
                    error({total_is_zero, Total, K, SubT})
            end;
        none ->
            none
    end.

choose_nth(N, {M, SubT}) ->
    M:seek(N - 1, SubT).

to_orddict_iter(_M, none) ->
    [];
to_orddict_iter(M, {K, V, Iter}) ->
    [{K, M:to_list(V)} | to_orddict_iter(M, gb_trees:next(Iter))].
