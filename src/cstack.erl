%% @doc Counting stack. Biggest items at the front.
%%      O(N) worst case for every operation.
-module(cstack).

-type(cstack(T) :: {Total :: non_neg_integer(),
                    KeyCount :: non_neg_integer(),
                    [{T, Count :: non_neg_integer()}]}).

-export([new/0, increment/2, increment/3, total/1,
         from_list/1, to_list/1, seek/2]).

-spec new() -> cstack(_).
new() ->
    {0, 0, []}.

-spec increment({T, non_neg_integer()}, cstack(T)) -> cstack(T).
increment({K, Inc}, Stack) ->
    increment(K, Inc, Stack).

-spec increment(T, non_neg_integer(), cstack(T)) -> cstack(T).
increment(K, Inc, {Total, KeyCount, L}) ->
    case lists:keyfind(K, 1, L) of
        {_, N} ->
            {Inc + Total, KeyCount, insort(K, Inc + N, true, L)};
        false ->
            {Inc + Total, 1 + KeyCount, insort(K, Inc, false, L)}
    end.

-spec to_list(cstack(T)) -> [{T, non_neg_integer()}].
to_list({_Total, _KC, L}) ->
    L.

-spec from_list([{T, non_neg_integer()}]) -> cstack(T).
from_list(L) ->
    %% Sum and reverse the value sorted list.
    lists:foldl(
      fun (P={_K, V}, {T, KC, Acc}) -> {V + T, 1 + KC, [P | Acc]} end,
      new(),
      lists:keysort(2, L)).

-spec total(cstack(_)) -> non_neg_integer().
total({Total, _KeyCount, _L}) ->
    Total.

-spec seek(non_neg_integer(), cstack(T)) -> T.
seek(N, {Total, _KeyCount, L}) when N < Total ->
    seek2(N, L).

seek2(N, [{K, V} | Rest]) ->
    case N - V of
        N1 when N1 >= 0 ->
            seek2(N1, Rest);
        _ ->
            K
    end.

insort(K, V, Remove, [P={_K, V1} | T]) when V < V1 ->
    [P | insort(K, V, Remove, T)];
insort(K, V, true, L) ->
    [{K, V} | lists:keydelete(K, 1, L)];
insort(K, V, false, L) ->
    [{K, V} | L].
