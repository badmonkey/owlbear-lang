
-module(beamy).

-export([main/1]).


%%%%% ------------------------------------------------------- %%%%%


pp_process_file(undefined, _) ->
    io:format("No file specified~n");
    
pp_process_file(FileName, TokFmt) ->
    io:format("~n----------------~n"),
    case preprocessor:file(FileName) of
        {ok, Tokens, _}         ->
            case TokFmt of
                true    ->
                    io:format("~p~n~n", [Tokens])
                    
            ;   false   ->
                    lists:foldl(
                            fun({Type, {L,_}}, L) ->
                                io:format("~p ", [Type]), L
                                
                            ;  ({Type, {L,_}}, _) ->
                                io:format("~n~p ", [Type]), L
                                
                            ;  ({'beamtools$chunkpp', {L,_}, _Data}, L) ->
                                io:format("CHUNK "), L
                                
                            ;  ({'beamtools$chunkpp', {L,_}, _Data}, _) ->
                                io:format("~nCHUNK "), L
                                
                            ;  ({_, {L,_}, Data}, L) ->
                                io:format("~p ", [Data]), L
                                
                            ;  ({_, {L,_}, Data}, _) ->
                                io:format("~n~p ", [Data]), L
                            end, 0, Tokens),
                            io:format("~n~n")
                    
            end
                    
    ;   {error, ErrorInfo, _}   -> io:format("ERROR ~p~n~n", [ErrorInfo])
    end.



beamy_pp(App, Args) ->
    Spec = consoleapp:build(App,
            [ {version, "0.1"}
            , {option, scan_fmt, boolean, $s, "Print output as erl_scan token lists"}
            , {positional, input, undefined, "File to run preprocessor on"}
            ]),
    Result = consoleapp:process(Spec, Args),
    Props = consoleapp:get_options(Result),
    UseTokFormat = proplists:get_bool(scan_fmt, Props),
    File = proplists:get_value(input, Props, undefined),
    pp_process_file(File, UseTokFormat).



main(Args) ->
    Spec = consoleapp:build(
            [ {appname, "beamy"}
            , {version, "1.0"}
            , {command, ["preprocess", "pp"], fun beamy_pp/2, "Output from Preprocessor"}
            ]),

    Result = consoleapp:process(Spec, Args),

    consoleapp:application_halt(Result, 0).
    
    
