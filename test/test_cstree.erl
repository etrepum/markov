-module(test_cstree).
-include_lib("eunit/include/eunit.hrl").

to_list_test() ->
    ?assertEqual(
       [{a, 1}, {b, 2}],
       lists:sort(
         cstree:to_list(lists:foldl(fun cstree:increment/2,
                                    cstree:new(),
                                    [{b, 1}, {a, 1}, {b, 1}])))),
    ok.

from_list_test() ->
    ?assertEqual(
       [{a, 2}, {b, 1}],
       lists:sort(cstree:to_list(cstree:from_list([{b, 1}, {a, 2}])))),
    ?assertEqual(
       [{a, 2}, {b, 1}],
       lists:sort(cstree:to_list(cstree:from_list([{a, 2}, {b, 1}])))),
    ?assertEqual(
       [{a, 2}, {b, 1}, {c, 4}, {d, 1}, {e, 6}],
       lists:sort(
         cstree:to_list(
           cstree:from_list(
             [{a, 2}, {b, 1}, {c, 4}, {d, 1}, {e, 6}])))),
    ok.

total_test() ->
    ?assertEqual(
       3,
       cstree:total(cstree:from_list([{a, 2}, {b, 1}]))),
    ok.

inc_test() ->
    ?assertEqual(
       [{a, 1}, {b, 1}],
       lists:sort(
         cstree:to_list(
           lists:foldl(fun cstree:increment/2,
                       cstree:new(),
                       [{a, 1}, {b, 1}])))),
    ?assertEqual(
       [{a, 2}, {b, 1}],
       lists:sort(
         cstree:to_list(
           lists:foldl(fun cstree:increment/2,
                       cstree:new(),
                       [{a, 1}, {b, 1}, {a, 1}])))),
    ?assertEqual(
       [{a, 2}, {b, 1}, {c, 4}],
       lists:sort(
         cstree:to_list(
           lists:foldl(fun cstree:increment/2,
                       cstree:new(),
                       [{a, 1}, {b, 1}, {a, 1}, {c, 1}, {c, 1},
                        {c, 1}, {c, 1}])))),
    ?assertEqual(
       [{a, 6}, {b, 5}, {c, 4}],
       lists:sort(
         cstree:to_list(
           lists:foldl(fun cstree:increment/2,
                       cstree:new(),
                       [{a, 1}, {b, 1}, {a, 1}, {c, 1}, {c, 1},
                        {c, 1}, {c, 1}, {b, 1}, {b, 1}, {b, 1},
                        {b, 1}, {a, 1}, {a, 1}, {a, 1}, {a, 1}])))),
    ok.

seek_test() ->
    S = cstree:from_list([{c, 4}, {a, 2}, {b, 1}]),
    ?assertEqual(
       lists:sort([c, c, c, c, a, a, b]),
       lists:sort([cstree:seek(N, S) || N <- lists:seq(0, 6)])),
    ok.
