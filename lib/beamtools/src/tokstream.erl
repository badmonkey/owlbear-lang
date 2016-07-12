
-module(tokstream).

-export([ new/1, make_embed_token/2
        , push_token/2, push_many_tokens/2, push_to_chunk/2
        , start_chunk/2, end_chunk/1
        , final/1, error/1]).


-type deep_tokens() :: [ type:token() | deep_tokens() ].



-record(tok_stream,
       { tag            = undefined :: atom()
       , stream         = []        :: deep_tokens()
       , chunk          = []        :: deep_tokens()
       , error          = undefined
       }).
       
-type stream() :: #tok_stream{}.

-export_type([stream/0]).       


%%%%% ------------------------------------------------------- %%%%%


new(Tag) -> #tok_stream{ tag = Tag }.


%%%%% ------------------------------------------------------- %%%%%


make_embed_token(_, []) ->
    [];
    
make_embed_token(Tag, Chunk)
        when is_atom(Tag), is_list(Chunk) ->    
    Data = lists:reverse(Chunk),
    %{Tag, element(2, hd(Data)), {embed, Data}}.
    {Tag, element(2, hd(Data)), Data}.


%%%%% ------------------------------------------------------- %%%%%
    

push_token(#tok_stream{ chunk = [], stream = Stream } = State, Token) ->
    State#tok_stream{ stream = [Token | Stream] };

push_token(#tok_stream{} = State, Token) ->
    State2 = end_chunk(State),
    push_token(State2, Token).

    
%%%%% ------------------------------------------------------- %%%%%


push_many_tokens(#tok_stream{} = State, Tokens)
        when is_list(Tokens) ->
    State2 = end_chunk(State),
    State2#tok_stream{ stream = lists:reverse(Tokens) ++ State2#tok_stream.stream }.
    

%%%%% ------------------------------------------------------- %%%%%


push_to_chunk(#tok_stream{ chunk = Chunk } = State, Tokens)
        when is_list(Tokens) ->
    State#tok_stream{ chunk = lists:reverse(Tokens) ++ Chunk };
    
push_to_chunk(#tok_stream{ chunk = Chunk } = State, Token) ->
    State#tok_stream{ chunk = [Token | Chunk] }.


%%%%% ------------------------------------------------------- %%%%%


start_chunk(#tok_stream{} = State, Tokens)
        when is_list(Tokens) ->
    State2 = end_chunk(State),
    State2#tok_stream{ chunk = lists:reverse(Tokens) }.
    

%%%%% ------------------------------------------------------- %%%%%


end_chunk(#tok_stream{ chunk = [] } = State) ->
    State;
    
end_chunk(#tok_stream{ tag = Tag, chunk = Chunk, stream = Stream } = State) ->
    ChunkToken = make_embed_token(Tag, Chunk),
    State#tok_stream{ stream = [ChunkToken | Stream], chunk = [] }.


%%%%% ------------------------------------------------------- %%%%%


final(#tok_stream{} = State) ->
    State2 = end_chunk(State),
    lists:flatten( lists:reverse(State2#tok_stream.stream) ).

    
%%%%% ------------------------------------------------------- %%%%%


error(#tok_stream{} = State) ->
    State#tok_stream{ error = {error, something} }.
    