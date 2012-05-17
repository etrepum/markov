-module(markov).
-export([new/0, new/1, new/2]).
-export([input/2, output/1, output/2, from_list/2, to_list/1]).

-define(WORD_MAX, 100).
-define(DEFAULT_STORAGE, markov_ets).

new() ->
    new(?DEFAULT_STORAGE, []).

new(M) when is_atom(M) ->
    new(M, []);
new(L) when is_list(L) ->
    new(?DEFAULT_STORAGE, L).

new(M, Opts) ->
    {M, M:new(Opts)}.

from_list(L, {M, T}) ->
    {M, M:from_list(L, T)}.

to_list({M, T}) ->
    M:to_list(T).

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

append(K, V, {M, T}) ->
    {M, M:append(K, V, T)}.

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

fetch(K={_A, B}, RState, Max, T={M, MT}) when Max > 0 ->
    case choose(M:lookup(K, MT), RState, M) of
        {stop, _} ->
            [];
        {C, RState1} ->
            [C | fetch({B, C}, RState1, Max - 1, T)]
    end;
fetch(_K, _RState, _Max, _T) ->
    [].

choose({value, {Count, SubT}}, RState, M) ->
    {N, RState1} = random:uniform_s(Count, RState),
    {M:choose_nth(N, SubT), RState1};
choose(none, RState, _M) ->
    {stop, RState}.

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
