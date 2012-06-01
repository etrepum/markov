%% @doc Counting stack. Biggest items at the front.
%%      O(N) worst case for every operation.
-module(cstack).

-type(cstack(T) :: {Total :: non_neg_integer(),
                    [{T, Count :: non_neg_integer()}]}).

-export([new/0, increment/2, increment/3, total/1,
         from_list/1, to_list/1, seek/2]).

-spec new() -> cstack(_).
new() ->
    {0, []}.

-spec increment({T, non_neg_integer()}, cstack(T)) -> cstack(T).
increment({K, Inc}, Stack) ->
    increment(K, Inc, Stack).

-spec increment(T, non_neg_integer(), cstack(T)) -> cstack(T).
increment(K, Inc, {Total, L}) ->
    {Inc + Total, inc(K, Inc, L)}.

-spec to_list(cstack(T)) -> [{T, non_neg_integer()}].
to_list({_Total, L}) ->
    L.

-spec from_list([{T, non_neg_integer()}]) -> cstack(T).
from_list(L) ->
    %% Sum and reverse the value sorted list.
    lists:foldl(
      fun (P={_K, V}, {T, Acc}) -> {V + T, [P | Acc]} end,
      new(),
      lists:keysort(2, L)).

-spec total(cstack(_)) -> non_neg_integer().
total({Total, _L}) ->
    Total.

-spec seek(non_neg_integer(), cstack(T)) -> T.
seek(N, {Total, L}) when N < Total ->
    seek2(N, L).

seek2(N, [{K, V} | Rest]) ->
    case N - V of
        N1 when N1 >= 0 ->
            seek2(N1, Rest);
        _ ->
            K
    end.

inc(K, Inc, L) ->
    case lists:keyfind(K, 1, L) of
        {_, N} ->
            insort(K, Inc + N, true, L);
        false ->
            insort(K, Inc, false, L)
    end.

insort(K, V, Remove, [P={_K, V1} | T]) when V < V1 ->
    [P | insort(K, V, Remove, T)];
insort(K, V, true, L) ->
    [{K, V} | lists:keydelete(K, 1, L)];
insort(K, V, false, L) ->
    [{K, V} | L].
