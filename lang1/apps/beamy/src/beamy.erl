
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
                                
                            ;  ({'beamtools$CHUNK', {L,_}, {embed, _Data}}, L) ->
                                io:format("CHUNK "), L
                                
                            ;  ({'beamtools$CHUNK', {L,_}, {embed, _Data}}, _) ->
                                io:format("~nCHUNK "), L
                                
                            ;   ({'beamtools$DIRECTIVE', {L,_}, {embed, Data}}, L) ->
                                {_, {_,_}, Directive} = Data,
                                io:format("USER<~p> ", [Directive]), L
                                
                            ;   ({'beamtools$DIRECTIVE', {L,_}, {embed, Data}}, _) ->
                                {_, {_,_}, Directive} = Data,
                                io:format("~nUSER<~p> ", [Directive]), L
                                
                            ;  ({Type, {L,_}, Data}, L) ->
                                io:format("~p[~p] ", [Type, Data]), L
                                
                            ;  ({Type, {L,_}, Data}, _) ->
                                io:format("~n~p[~p] ", [Type, Data]), L
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
    
    
