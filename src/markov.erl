-module(markov).
-export([new/0, input/2, output/1, from_orddict/1, to_orddict/1]).

-define(WORD_MAX, 100).

new() ->
    gb_trees:empty().

from_orddict(L) ->
    gb_trees:from_orddict([{K, subtree_from_orddict(V)} || {K, V} <- L]).

to_orddict(T) ->
    to_orddict_iter(gb_trees:next(gb_trees:iterator(T))).

input(B, T) when is_binary(B) ->
    input_tokens([Word || Word <- re:split(B, "\\s+", [{return, binary}]),
                          Word =/= <<" ">>],
          T).

output(T) ->
    case (<<<<" ", W/binary>> || W <- fetch(?WORD_MAX, T)>>) of
        <<" ", String/binary>> ->
            String;
        <<>> ->
            <<>>
    end.

input_tokens([], T) ->
    T;
input_tokens(L, T) ->
    input(L, {start, start}, T).

input([C | Rest], {A, B}, T) ->
    input(Rest, {B, C}, append({A, B}, C, T));
input([], {A, B}, T) ->
    append({A, B}, stop, T).

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

fetch(Max, T) ->
    fetch([], Max, T).

fetch(L, Max, T) ->
    fetch(case L of
              [] -> {start, start};
              [A] -> {start, A};
              [A, B] -> {A, B}
          end,
          erlang:now(),
          Max,
          T).

fetch(K={_A, B}, RState, Max, T) when Max > 0 ->
    case choose(gb_trees:lookup(K, T), RState) of
        {stop, _} ->
            [];
        {C, RState1} ->
            [C | fetch({B, C}, RState1, Max - 1, T)]
    end.

choose({value, {Count, SubT}}, RState) ->
    {N, RState1} = random:uniform_s(Count, RState),
    {choose_nth(N, gb_trees:next(gb_trees:iterator(SubT))), RState1};
choose(none, RState) ->
    {stop, RState}.

choose_nth(N, {_K, V, Iter}) when N > V ->
    choose_nth(N - V, gb_trees:next(Iter));
choose_nth(_N, {K, _V, _Iter}) ->
    K.

subtree_from_orddict(L) ->
    T = gb_trees:from_orddict(L),
    {subtree_count(gb_trees:next(gb_trees:iterator(T)), 0), T}.

subtree_count(none, Acc) ->
    Acc;
subtree_count({_K, V, Iter}, Acc) ->
    subtree_count(gb_trees:next(Iter), V + Acc).

to_orddict_iter(none) ->
    [];
to_orddict_iter({K, V, Iter}) ->
    [{K, to_orddict_subtree(V)} | to_orddict_iter(gb_trees:next(Iter))].

to_orddict_subtree({_Count, T}) ->
    gb_trees:to_list(T).
