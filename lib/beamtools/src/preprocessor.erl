
-module(preprocessor).

-export([ file/1, file/2, process/2 ]).


%%%%% ------------------------------------------------------- %%%%%


-record(pp_state,
       { something
       , scanner                :: type:scanner()
       , macros                 :: #{ string() => type:tokens() }
       , replace_wrapper        :: fun( ( type:tokens() ) -> type:tokens() )
       , resolver
       }).

       
-define(TOKEN_CHUNK, 'beamtools$chunkpp').
       
-define(PATTERN_DASH, {'-',_}).    
-define(PATTERN_QMARK, {'?',_}).
-define(PATTERN_LBRACKET, {'(',_}).
-define(PATTERN_RBRACKET, {')',_}).
-define(PATTERN_COMMA, {',',_}).
-define(PATTERN_DOT, {dot,_}).
-define(PATTERN_ATOM(X), {atom,_,X}).
-define(PATTERN_VAR(X), {var,_,X}).
       

%%%%% ------------------------------------------------------- %%%%%

%
% filename
% module
% scanner
%

file(FileName) ->
    file( FileName
        , #{ filename => FileName
           , scanner  => fun erl_scan:string/2
           } ).
           

file(FileName, #{ scanner := Scanner } = Opts) ->
    {ok, FileContents} = file:read_file(FileName),
    Data = binary_to_list(FileContents),
    Output = Scanner(Data, {1,1}),
    process(Output, new(Opts));
    
file(FileName, #{} = Opts) ->
    file(FileName, Opts#{ scanner => fun erl_scan:string/2 }).


%%%%% ------------------------------------------------------- %%%%%


new(#{} = Opts) ->
    #pp_state{}.
    

%%%%% ------------------------------------------------------- %%%%%


-spec process( type:token_return(), #pp_state{} ) -> type:token_return().

process({error, _ErrorInfo, _} = Error, _State) ->
    Error;
    
process({ok, Tokens, EndLoc}, _State ) ->
    {ok, filter_tokens(Tokens, tokstream:new(?TOKEN_CHUNK)), EndLoc}.


%%%%% ------------------------------------------------------- %%%%%


% scan list of tokens for interesting tokens (hiding the rest in fake 'chunk' tokens).

filter_tokens([], Stream) ->
    tokstream:final(Stream);

filter_tokens([?PATTERN_QMARK = TokQMark, ?PATTERN_ATOM(_) = TokName | Rest], Stream) ->
    {RestInput, Stream2} = scan_macro_use(Rest, tokstream:push_many_tokens(Stream, [TokQMark, TokName])),
    filter_tokens(RestInput, Stream2);
    
filter_tokens([?PATTERN_QMARK = TokQMark, ?PATTERN_VAR(_) = TokName | Rest], Stream) ->
    {RestInput, Stream2} = scan_macro_use(Rest, tokstream:push_many_tokens(Stream, [TokQMark, TokName])),
    filter_tokens(RestInput, Stream2);

filter_tokens([?PATTERN_DASH = TokDash, ?PATTERN_ATOM(Directive) = TokName | Rest], Stream) ->
    {RestInput, Stream2} =  case lists:member(Directive, directives()) of
                                true    -> scan_directive(Directive, Rest, tokstream:push_many_tokens(Stream, [TokDash, TokName]))
                            ;   false   -> chunk_tokens(Rest, tokstream:start_chunk(Stream, [TokDash, TokName]))
                            end,
    filter_tokens(RestInput, Stream2);

filter_tokens(Input, Stream) ->
    {RestInput, Stream2} = chunk_tokens(Input, Stream),
    filter_tokens(RestInput, Stream2).


%%%%% ------------------------------------------------------- %%%%%


scan_macro_use([], Stream) ->
    {[], Stream};
    
scan_macro_use([?PATTERN_LBRACKET = TokLeft | Rest], Stream) ->
    {RestInput, Stream2} = chunk_exprs(Rest, tokstream:push_token(Stream, TokLeft), 0),
    case RestInput of
        [?PATTERN_RBRACKET = TokRight | Rest2] ->
            {Rest2, tokstream:push_token(Stream2, TokRight)}
            
    ;   _               ->
            chunk_tokens(RestInput, Stream2)
    end;

scan_macro_use(Input, Stream) ->
    {Input, Stream}.


%%%%% ------------------------------------------------------- %%%%%


scan_directive(_, [], Stream) ->
    {[], Stream};
    
    
scan_directive('else', [?PATTERN_DOT = TokDot | Rest], Stream) ->
    {Rest, tokstream:push_token(Stream, TokDot)};
    
scan_directive('endif', [?PATTERN_DOT = TokDot | Rest], Stream) ->
    {Rest, tokstream:push_token(Stream, TokDot)};
    
    
scan_directive('undef', Input, Stream) ->
    match_macro_name(Input, Stream);
    
scan_directive('ifdef', Input, Stream) ->
    match_macro_name(Input, Stream);
    
scan_directive('ifndef', Input, Stream) ->
    match_macro_name(Input, Stream);
    
    
scan_directive('module', Input, Stream) ->
    match_macro_name(Input, Stream);        % move out of pp grammar

    
scan_directive(_, Input, Stream) ->
    chunk_tokens(Input, Stream).     % likely force an error in the next stage
    
    
match_macro_name([?PATTERN_LBRACKET = TokLeft, ?PATTERN_ATOM(_) = TokName, ?PATTERN_RBRACKET = TokRight, ?PATTERN_DOT = TokDot | Rest], Stream) ->
    {Rest, tokstream:push_many_tokens(Stream, [TokLeft, TokName, TokRight, TokDot])};
    
match_macro_name([?PATTERN_LBRACKET = TokLeft, ?PATTERN_VAR(_) = TokName, ?PATTERN_RBRACKET = TokRight, ?PATTERN_DOT = TokDot | Rest], Stream) ->
    {Rest, tokstream:push_many_tokens(Stream, [TokLeft, TokName, TokRight, TokDot])};
    
match_macro_name(Input, Stream) ->
    chunk_tokens(Input, Stream).    %% @todo not sure, force in parser?


%%%%% ------------------------------------------------------- %%%%%


chunk_tokens([], Stream) ->
    {[], tokstream:end_chunk(Stream)};
    
chunk_tokens([?PATTERN_QMARK, ?PATTERN_ATOM(_) | _Rest] = Input, Stream) ->
    {Input, tokstream:end_chunk(Stream)};
    
chunk_tokens([?PATTERN_QMARK, ?PATTERN_VAR(_) | _Rest] = Input, Stream) ->
    {Input, tokstream:end_chunk(Stream)};

    
chunk_tokens([?PATTERN_DASH = TokDash, ?PATTERN_ATOM(Directive) = TokName | Rest] = Input, Stream) ->
    case lists:member(Directive, directives()) of
        true    -> {Input, tokstream:end_chunk(Stream)}
    ;   false   ->
            Stream2 = tokstream:push_to_chunk(Stream, [TokDash, TokName]),
            chunk_tokens(Rest, Stream2)
    end;

    
chunk_tokens([Hd | Rest], Stream) ->
    chunk_tokens(Rest, tokstream:push_to_chunk(Stream, Hd)).
    

%%%%% ------------------------------------------------------- %%%%%

% -what(...(...)...).
% -what(...(...)).
% -what(..., ...(...)..., ...).

% @todo change to handle multiple sets of balanced chars for comma handling
% ( )  [ ]  { }   < >

chunk_exprs([], Stream, 0) ->
    {[], tokstream:end_chunk(Stream) };

chunk_exprs([], Stream, _Depth) ->
    {[], tokstream:error(Stream)};  % @todo error message


chunk_exprs([?PATTERN_LBRACKET = TokLeft | Rest], Stream, Depth) ->
    chunk_exprs(Rest, tokstream:push_to_chunk(Stream, TokLeft), Depth + 1);

chunk_exprs([?PATTERN_RBRACKET | _Rest] = Input, Stream, 0) ->
    {Input, tokstream:end_chunk(Stream) };

chunk_exprs([?PATTERN_RBRACKET = TokRight | Rest], Stream, Depth) ->
    chunk_exprs(Rest, tokstream:push_to_chunk(Stream, TokRight), Depth - 1);

%chunk_exprs([?PATTERN_COMMA = TokComma | Rest], Stream, 0) ->
%    chunk_exprs(Rest, tokstream:push_token(Stream, TokComma), 0);


chunk_exprs([?PATTERN_QMARK = TokQMark, ?PATTERN_ATOM(_) = TokName | Rest], Stream, Depth) ->
    {RestInput, Stream2} = scan_macro_use(Rest, tokstream:push_many_tokens(Stream, [TokQMark, TokName])),
    chunk_exprs(RestInput, Stream2, Depth);
    
chunk_exprs([?PATTERN_QMARK = TokQMark, ?PATTERN_VAR(_) = TokName | Rest], Stream, Depth) ->
    {RestInput, Stream2} = scan_macro_use(Rest, tokstream:push_many_tokens(Stream, [TokQMark, TokName])),
    chunk_exprs(RestInput, Stream2, Depth);

chunk_exprs([?PATTERN_QMARK = TokQMark, ?PATTERN_QMARK = TokQMark2, ?PATTERN_VAR(_) = TokName | Rest], Stream, Depth) ->
    chunk_exprs(Rest, tokstream:push_many_tokens(Stream, [TokQMark, TokQMark2, TokName]), Depth);


chunk_exprs([Hd | Rest], Stream, Depth) ->
    chunk_exprs(Rest, tokstream:push_to_chunk(Stream, Hd), Depth).


    

%%%%% ------------------------------------------------------- %%%%%


directives() ->
    [include, include_lib, define, ifdef, ifndef, undef, else, endif, error, warning, module, file].
    
% move module from internal to user parsing
    
% implement in phase 2    
% if, elif    
