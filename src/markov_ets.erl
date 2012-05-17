-module(markov_ets).
-export([new/0, from_list/1, from_list/2, to_list/1, append/3, lookup/2,
         choose_nth/2]).

-define(WORD_MAX, 100).

new() ->
    %% {{{token(), token()}, pair}, integer()}
    %% {{{token(), token()}, token()}, integer()}
    ets:new(?MODULE, [ordered_set]).

from_list(L) ->
    from_list(L, new()).

from_list([{K, V} | Rest], T) ->
    Total = lists:foldl(
              fun ({Token, C}, Acc) ->
                      true = ets:insert_new(T, {{K, Token}, C}),
                      C + Acc
              end,
              0,
              V),
    true = ets:insert_new(T, {{K, pair}, Total}),
    from_list(Rest, T);
from_list([], T) ->
    T.

to_list(T) ->
    [{K, unroll(K, T)}
     || K <- ets:select(T, [{{{'$1',pair},'_'},[],['$1']}])].

unroll(K, T) ->
    ets:select(
      T,
      [{{{K, '$1'}, '$2'}, [{'=/=', '$1', pair}], [{{'$1', '$2'}}]}]).

inc(T, K) ->
    try ets:update_counter(T, K, 1)
    catch error:badarg ->
            ets:insert_new(T, {K, 1}),
            1
    end.

append(K, V, T) ->
    inc(T, {K, pair}),
    inc(T, {K, V}),
    T.

lookup(K, T) ->
    try ets:lookup_element(T, {K, pair}, 2)
    of C ->
            {value, {C, {K, T}}}
    catch error:badarg ->
            none
    end.

choose_nth(N, {K, T}) ->
    %% NOTE: It's important that 'pair' sorts lower than
    %%       'start', 'stop', and any binary!
    choose_nth_iter(N, ets:next(T, {K, pair}), T).

choose_nth_iter(N, K={_Prev, Next}, T) ->
    case ets:lookup_element(T, K, 2) of
        V when N > V ->
            choose_nth_iter(N - V, ets:next(T, K), T);
        _ ->
            Next
    end.
