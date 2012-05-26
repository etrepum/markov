%% @doc A Linear Algorithm For Generating Random Numbers With a Given
%% Distribution.
%%
%% See:
%%
%%   http://web.eecs.utk.edu/~vose/Publications/random.pdf
%%   http://www.keithschwarz.com/darts-dice-coins/
%%   http://alaska-kamtchatka.blogspot.com/2011/12/voses-alias-method.html
%%
-module(vose).
-export([new/1, choose/2]).

new(Pairs=[_|_]) ->
    {Len, Sum} = lists:foldl(
                   fun ({_, Freq}, {N, Acc}) -> {1 + N, Freq + Acc} end,
                   {0, 0},
                   Pairs),
    init(partition(Pairs, Sum, Len), array:new(Len)).

choose(Random, Vose) ->
    U = Random * array:size(Vose),
    J = trunc(U),
    P = U - J,
    case array:get(J, Vose) of
        {Prob, Item, _Alias} when P =< Prob ->
            Item;
        {_Prob, _Item, Alias} ->
            Alias
    end.

init({[{L, LWeight, LItem} | Small],
      [{G, GWeight, GItem} | Large]},
     Prob) ->
    GWeight1 = (LWeight + GWeight) - 1,
    GT = {G, GWeight1, GItem},
    init(
      case GWeight1 > 1 of
          true ->
              {Small, [GT | Large]};
          false ->
              {[GT | Small], Large}
      end,
      array:set(L, {LWeight, LItem, GItem}, Prob));
init({Small, Large}, Prob) ->
    lists:foldl(fun ({N, _NWeight, NItem}, Acc) ->
                        array:set(N, {1, NItem, undefined}, Acc)
                end,
                Prob,
                lists:append(Small, Large)).

partition(Pairs, Sum, Len) ->
    Scale = Len / Sum,
    Cutoff = Sum / Len,
    F = fun ({Item, Freq}, {N, {Small, Large}}) ->
                Weight = Freq * Scale,
                {1 + N,
                 case Freq > Cutoff of
                     true ->
                         {Small, [{N, Weight, Item} | Large]};
                     false ->
                         {[{N, Weight, Item} | Small], Large}
                 end}
        end,
    {_, Worklists} = lists:foldl(F, {0, {[], []}}, Pairs),
    Worklists.
