%% @doc Counting splay tree.
-module(cstree).

-type(cstree_node(T) :: {Left :: cstree(T),
                         Right :: cstree(T),
                         Value :: T,
                         ElemCount :: non_neg_integer(),
                         TotalCount :: non_neg_integer()}).

-type(cstree(T) :: empty | cstree_node(T)).
-export([new/0, increment/2, increment/3, total/1, count/1,
         from_list/1, to_list/1, seek/2]).
-define(E, empty).

-spec new() -> cstree(_).
new() ->
    ?E.

-spec from_list([{T, non_neg_integer()}]) -> cstree(T).
from_list(L) ->
    lists:foldl(fun increment/2, new(), L).

-spec to_list(cstree(T)) -> [{T, non_neg_integer()}].
to_list(T) ->
    to_list(T, [], []).

-spec increment({T, non_neg_integer()}, cstree(T)) -> cstree(T).
increment({Elem, Inc}, T) ->
    increment(Elem, Inc, T).

-spec increment(T, non_neg_integer(), cstree(T)) -> cstree(T).
increment(Elem, Inc, T) ->
    case partition(Elem, T) of
        {{Left, Right, X, XC, _XTC}, Big} when X =:= Elem ->
            Right = ?E,
            t(Left, Big, X, Inc + XC);
        {Small, Big} ->
            t(Small, Big, Elem, Inc)
    end.

-spec total(cstree(_)) -> non_neg_integer().
total({_Left, _Right, _Elem, _ElemCount, Total}) ->
    Total;
total(?E) ->
    0.

-spec count(cstree(_)) -> non_neg_integer().
count({_Left, _Right, _Elem, ElemCount, _Total}) ->
    ElemCount;
count(?E) ->
    0.

-spec seek(non_neg_integer(), cstree_node(T)) -> T.
seek(N, {L, R, X, XC, _XTC}) ->
    case N - total(L) of
        NL when NL >= 0 ->
            case NL - XC of
                NR when NR >= 0 ->
                    seek(NR, R);
                _ ->
                    X
            end;
        _ ->
            seek(N, L)
    end.

partition(Pivot, T={Left, Right, X, XC, _XTC}) when X =< Pivot ->
    case Right of
        {Right1, Right2, Y, YC, _YTC} ->
            case Y =< Pivot of
                true ->
                    {Small, Big} = partition(Pivot, Right2),
                    {t(t(Left, Right1, X, XC), Small, Y, YC), Big};
                false ->
                    {Small, Big} = partition(Pivot, Right1),
                    {t(Left, Small, X, XC), t(Big, Right2, Y, YC)}
            end;
        ?E ->
            {T, Right}
    end;
partition(Pivot, T={Left, Right, X, XC, _TC}) ->
    case Left of
        {Left1, Left2, Y, YC, _YTC} ->
            case Y =< Pivot of
                true ->
                    {Small, Big} = partition(Pivot, Left2),
                    {t(Left1, Small, Y, YC), t(Big, Right, X, XC)};
                false ->
                    {Small, Big} = partition(Pivot, Left1),
                    {Small, t(Big, t(Left2, Right, X, XC), Y, YC)}
            end;
        ?E ->
            {Left, T}
    end;
partition(_Pivot, T=?E) ->
    {T, T}.

t(Left, Right, Elem, ElemCount) ->
    {Left, Right, Elem, ElemCount, total(Left) + total(Right) + ElemCount}.

to_list(T={_Left, Right, _X, _XC, _XTC}, Stack, Acc) ->
    to_list(Right, [T | Stack], Acc);
to_list(?E, [{Left, _Right, X, XC, _XTC} | Stack], Acc) ->
    to_list(Left, Stack, [{X, XC} | Acc]);
to_list(?E, [], Acc) ->
    Acc.
