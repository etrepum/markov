-module(markov).
-export([new/0, input/2, output/1, output/2, from_list/1, to_list/1]).

-define(WORD_MAX, 100).

new() ->
    gb_trees:empty().

from_list(L) ->
    lists:foldl(fun ({K, V}, T) ->
                        gb_trees:insert(K, subtree_from_list(V), T)
                end,
                gb_trees:empty(),
                L).

to_list(T) ->
    to_orddict_iter(gb_trees:next(gb_trees:iterator(T))).

input(B, T) when is_binary(B) ->
    input_tokens([Word || Word <- re:split(B, "\\s+", [{return, binary}]),
                          Word =/= <<" ">>],
          T).

output(T) ->
    output([], T).

output(L, T) ->
    case (<<<<" ", W/binary>> || W <- fetch(L, ?WORD_MAX, T)>>) of
        <<" ", String/binary>> ->
            redact(String);
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

fetch(L, Max, T) ->
    {Pair, Next} = case L of
                       [] -> {{start, start}, undefined};
                       [A] -> {{start, A}, []};
                       [A, B | _] -> {{A, B}, [A]}
                   end,
    case fetch(Pair, erlang:now(), Max, T) of
        [] when Next =/= undefined ->
            fetch(Next, Max, T);
        Res ->
            Res
    end.

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

redact(S) ->
    re:replace(
      S,
      <<$(,
        "\\b\\d{3}[ .-]\\d{4}\\b", $|,
        "\\b[iaeou][thor]?[aeiou][cartman]?[duh]?[rutoh][rutoh]mochi\\b", $|,
        "\\b(?:https?://)?[^/]*mochi[^/]*/admin(?:/[^\s]*)?\\b",
        $)>>,
      <<"[REDACTED]">>,
      [global, {return, binary}]).
