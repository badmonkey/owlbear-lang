
-module(baron).

-export([main/1]).


%%%%% ------------------------------------------------------- %%%%%


debug_form(String) ->
    io:format("~n----------------~n"),
    io:format("~s~n~n", [String]),
    Form = absform:from_string(String),
    io:format("~s~n~n", [io_lib:print(Form)] ),
    Output = erl_pp:form(Form),
    io:format(Output),
    ok.


main(Args) ->
    io:format("Baron Compiler...~n"),
    [ debug_form(F) || F <- Args ].
    
