
-module(test1).
-version("1.0.0").


-ifdef(VERSION).

version() ->
    {?VERSION, ?major, ?minor}.
    
-comment(first, ?MODULE).

-else.

-define(TEST(X), ??X).

version() ->
    ?ADVVERSION( abc
               , ?MODULE
               , ?TEST(1)
               , 3 + (4 + 7)    % brackets need to balance
               , {1,1}          % test for comma splitting tuples
               , [1,2,3] ).     % test for comma splitting lists

-endif.

-define(FIN,"fin").
