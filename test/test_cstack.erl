-module(test_cstack).
-include_lib("eunit/include/eunit.hrl").

to_list_test() ->
    ?assertEqual(
       [{b, 2}, {a, 1}],
       cstack:to_list(lists:foldl(fun cstack:increment/2,
                                  cstack:new(),
                                  [{b, 1}, {a, 1}, {b, 1}]))),
    ok.

from_list_test() ->
    ?assertEqual(
       {3, 2, [{a, 2}, {b, 1}]},
       cstack:from_list([{b, 1}, {a, 2}])),
    ?assertEqual(
       {3, 2, [{a, 2}, {b, 1}]},
       cstack:from_list([{a, 2}, {b, 1}])),
    ok.

total_test() ->
    ?assertEqual(
       3,
       cstack:total(cstack:from_list([{a, 2}, {b, 1}]))),
    ok.

inc_test() ->
    ?assertEqual(
       {2, 2, [{b, 1}, {a, 1}]},
       lists:foldl(fun cstack:increment/2,
                   cstack:new(),
                   [{a, 1}, {b, 1}])),
    ?assertEqual(
       {3, 2, [{a, 2}, {b, 1}]},
       lists:foldl(fun cstack:increment/2,
                   cstack:new(),
                   [{a, 1}, {b, 1}, {a, 1}])),
    ?assertEqual(
       {7, 3, [{c, 4}, {a, 2}, {b, 1}]},
       lists:foldl(fun cstack:increment/2,
                   cstack:new(),
                   [{a, 1}, {b, 1}, {a, 1}, {c, 1}, {c, 1}, {c, 1}, {c, 1}])),
    ok.

seek_test() ->
    S = cstack:from_list([{c, 4}, {a, 2}, {b, 1}]),
    ?assertEqual(
       [c, c, c, c, a, a, b],
       [cstack:seek(N, S) || N <- lists:seq(0, 6)]),
    ok.
