-module(markov).
-export([new/0, input/2, fetch/2]).

new() ->
    append({start, start}, stop, gb_trees:empty()).

input(L, T) ->
    input(L, {start, start}, T).

input([C | Rest], {A, B}, T) ->
    input(Rest, {B, C}, append({A, B}, C, T));
input([], {A, B}, T) ->
    append({A, B}, stop, T).

append(K, V, T) ->
    case gb_trees:lookup(K, T) of
        none ->
            gb_trees:insert(K, {1, [V]}, T);
        {value, {Count, Others}} ->
            gb_trees:update(K, {1 + Count, [V | Others]}, T)
    end.

fetch(Max, T) ->
    fetch({start, start}, erlang:now(), Max, T).

fetch(K={_A, B}, RState, Max, T) when Max > 0 ->
    case choose(gb_trees:lookup(K, T), RState) of
        {stop, _} ->
            [];
        {C, RState1} ->
            [C | fetch({B, C}, RState1, Max - 1, T)]
    end.

choose({value, {Count, L}}, RState) ->
    {N, RState1} = random:uniform_s(Count, RState),
    {lists:nth(N, L), RState1};
choose(none, RState) ->
    {stop, RState}.
