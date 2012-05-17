-module(markov_ets).
-export([new/1, from_list/2, to_list/1, append/3, lookup/2, choose_nth/2]).

-define(WORD_MAX, 100).

new(Opts) ->
    %% {{{token(), token()}, pair}, integer()}
    %% {{{token(), token()}, token()}, integer()}
    {Ets, Name, Opts1} = lists:foldr(fun optfold/2,
                                     {ets, ?MODULE, [ordered_set]},
                                     Opts),
    {Ets, Ets:new(Name, Opts1)}.

optfold({module, Mod}, {_Mod, Name, Acc}) ->
    {Mod, Name, Acc};
optfold({name, Name}, {Mod, _Name, Acc}) ->
    {Mod, Name, Acc};
optfold(Opt, {Mod, Name, Acc}) ->
    {Mod, Name, [Opt | Acc]}.

from_list([{K, V} | Rest], State={Ets, T}) ->
    Total = lists:foldl(
              fun ({Token, C}, Acc) ->
                      true = Ets:insert_new(T, {{K, Token}, C}),
                      C + Acc
              end,
              0,
              V),
    true = Ets:insert_new(T, {{K, pair}, Total}),
    from_list(Rest, State);
from_list([], State) ->
    State.

to_list(State={Ets, T}) ->
    [{K, unroll(K, State)}
     || K <- Ets:select(T, [{{{'$1',pair},'_'},[],['$1']}])].

unroll(K, {Ets, T}) ->
    Ets:select(
      T,
      [{{{K, '$1'}, '$2'}, [{'=/=', '$1', pair}], [{{'$1', '$2'}}]}]).

inc({Ets, T}, K) ->
    try Ets:update_counter(T, K, 1)
    catch error:badarg ->
            Ets:insert_new(T, {K, 1}),
            1
    end.

append(K, V, State) ->
    inc(State, {K, pair}),
    inc(State, {K, V}),
    State.

lookup(K, State={Ets, T}) ->
    try Ets:lookup_element(T, {K, pair}, 2)
    of C ->
            {value, {C, {K, State}}}
    catch error:badarg ->
            none
    end.

choose_nth(N, {K, State={Ets, T}}) ->
    %% NOTE: It's important that 'pair' sorts lower than
    %%       'start', 'stop', and any binary!
    choose_nth_iter(N, Ets:next(T, {K, pair}), State).

choose_nth_iter(N, K={_Prev, Next}, State={Ets, T}) ->
    case Ets:lookup_element(T, K, 2) of
        V when N > V ->
            choose_nth_iter(N - V, Ets:next(T, K), State);
        _ ->
            Next
    end.
