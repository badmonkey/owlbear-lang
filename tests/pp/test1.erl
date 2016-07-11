
-module(test1).
-version("1.0.0").


-ifdef(VERSION).

version() ->
    {?VERSION, ?major, ?minor}.

-else.

version() ->
    ?ADVVERSION(1, (4 + 7), "beta", {1,1}).

-endif.

-define(FIN,"fin").
