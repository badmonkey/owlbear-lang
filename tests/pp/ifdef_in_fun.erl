
-module(ifdef_in_fun).

-export([ f/0 ]).


f() ->
    A =
-ifdef(TEST).
        test
-else.
        notest
-endif.
    , A.


