
-module(ifdef_in_fun).

-export([ f/0 ]).
-test(?MODULE).
-comment().


f() ->
    A =
-ifdef(TEST).
        test
-else.
        notest
-endif.
    , A.


