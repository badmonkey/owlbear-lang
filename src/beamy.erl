
-module(beamy).

-export([main/1]).


%%%%% ------------------------------------------------------- %%%%%


pp_process_file(FileName) ->
    io:format("~n----------------~n"),
    case preprocessor:file(FileName) of
        {ok, Tokens, _}         -> io:format("~p~n~n", [Tokens])
    ;   {error, ErrorInfo, _}   -> io:format("ERROR ~p~n~n", [ErrorInfo])
    end.



beamy_pp(App, Args) ->
    Spec = consoleapp:build(App,
            [ {version, "0.1"}
            , {positional, input, undefined, "File to run preprocessor on", fun pp_process_file/1}
            ]),
    _Result = consoleapp:process(Spec, Args),
    io:format("Running ~p~n", [ consoleapp:get_command(_Result) ]).



main(Args) ->
    Spec = consoleapp:build(
            [ {appname, "beamy"}
            , {version, "1.0"}
            , {command, ["preprocess", "pp"], fun beamy_pp/2, "Output from Preprocessor"}
            ]),

    Result = consoleapp:process(Spec, Args),

    consoleapp:application_halt(Result, 0).
    
    
