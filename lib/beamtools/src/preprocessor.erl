
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
    {ok, filter_tokens(Tokens, []), EndLoc}.


%%%%% ------------------------------------------------------- %%%%%


% scan list of tokens for interesting tokens (hiding the rest in fake 'chunk' tokens).

filter_tokens([], Stream) ->
    lists:flatten( lists:reverse(Stream) );

filter_tokens([?PATTERN_QMARK = TokQMark, ?PATTERN_ATOM(_) = TokName | Rest], Stream) ->
    {Chunk, RestInput} = scan_macro_use(Rest, [TokName, TokQMark]),
    filter_tokens(RestInput, [Chunk | Stream]);
    
filter_tokens([?PATTERN_QMARK = TokQMark, ?PATTERN_VAR(_) = TokName | Rest], Stream) ->
    {Chunk, RestInput} = scan_macro_use(Rest, [TokName, TokQMark]),
    filter_tokens(RestInput, [Chunk | Stream]);
    
filter_tokens([?PATTERN_DASH = TokDash, ?PATTERN_ATOM(Directive) = TokName | Rest], Stream) ->
    {Chunk, RestInput} =    case lists:member(Directive, directives()) of
                                true    -> scan_directive(Directive, Rest, [TokName, TokDash])
                            ;   false   -> chunk_tokens(Rest, [TokName, TokDash])
                            end,
    filter_tokens(RestInput, [Chunk | Stream]);

filter_tokens(Input, Stream) ->
    {Chunk, RestInput} = chunk_tokens(Input, []),
    filter_tokens(RestInput, [Chunk | Stream]).


%%%%% ------------------------------------------------------- %%%%%


scan_macro_use([], Chunk) ->
    {lists:reverse(Chunk), []};
    
scan_macro_use([?PATTERN_LBRACKET = TokLeft | Rest], Chunk) ->
    {lists:reverse([ TokLeft | Chunk]), Rest};

scan_macro_use(Input, Chunk) ->
    {lists:reverse(Chunk), Input}.


%%%%% ------------------------------------------------------- %%%%%


scan_directive(_, [], Chunk) ->
    {lists:reverse(Chunk), []};
    
    
scan_directive('else', [?PATTERN_DOT = TokDot | Rest], Chunk) ->
    {lists:reverse([TokDot | Chunk]), Rest};
    
scan_directive('endif', [?PATTERN_DOT = TokDot | Rest], Chunk) ->
    {lists:reverse([TokDot | Chunk]), Rest};
    
    
scan_directive('undef', Input, Chunk) ->
    {NewChunk, RestInput} = match_macro_name(Input),
    {lists:reverse([NewChunk | Chunk]), RestInput};
    
scan_directive('ifdef', Input, Chunk) ->
    {NewChunk, RestInput} = match_macro_name(Input),
    {lists:reverse([NewChunk | Chunk]), RestInput};
    
scan_directive('ifndef', Input, Chunk) ->
    {NewChunk, RestInput} = match_macro_name(Input),
    {lists:reverse([NewChunk | Chunk]), RestInput};

    
scan_directive(_, Input, Chunk) ->
    chunk_tokens(Input, Chunk).     % likely force an error in the next stage
    
    
match_macro_name([?PATTERN_LBRACKET = TokLeft, ?PATTERN_ATOM(_) = TokName, ?PATTERN_RBRACKET = TokRight, ?PATTERN_DOT = TokDot | Rest]) ->
    {[TokLeft, TokName, TokRight, TokDot], Rest};
    
match_macro_name([?PATTERN_LBRACKET = TokLeft, ?PATTERN_VAR(_) = TokName, ?PATTERN_RBRACKET = TokRight, ?PATTERN_DOT = TokDot | Rest]) ->
    {[TokLeft, TokName, TokRight, TokDot], Rest};
    
match_macro_name(Input) ->
    chunk_tokens(Input, []).


%%%%% ------------------------------------------------------- %%%%%


make_chunk_token([]) ->
    [];
    
make_chunk_token(Chunk) ->    
    Data = lists:reverse(Chunk),
    {?TOKEN_CHUNK, element(2, hd(Data)), Data}.
    

chunk_tokens([], Chunk) ->
    {make_chunk_token(Chunk), []};
    
chunk_tokens([?PATTERN_QMARK, ?PATTERN_ATOM(_) | _Rest] = Input, Chunk) ->
    {make_chunk_token(Chunk), Input};
    
chunk_tokens([?PATTERN_QMARK, ?PATTERN_VAR(_) | _Rest] = Input, Chunk) ->
    {make_chunk_token(Chunk), Input};    
    
chunk_tokens([?PATTERN_QMARK, ?PATTERN_QMARK, ?PATTERN_VAR(_) | _Rest] = Input, Chunk) ->
    {make_chunk_token(Chunk), Input};  
    
chunk_tokens([?PATTERN_DASH = TokDash, ?PATTERN_ATOM(Directive) = TokName | Rest] = Input, Chunk) ->
    case lists:member(Directive, directives()) of
        true    -> {make_chunk_token(Chunk), Input}
    ;   false   -> chunk_tokens(Rest, [TokName, TokDash | Chunk])   % pushed in reverse order
    end;

chunk_tokens([Hd | Rest], Chunk) ->
    chunk_tokens(Rest, [Hd | Chunk]).
    

%%%%% ------------------------------------------------------- %%%%%


directives() ->
    [include, include_lib, define, ifdef, ifndef, undef, else, endif, error, warning, module, file].
    
% implement in phase 2    
% if, elif    
