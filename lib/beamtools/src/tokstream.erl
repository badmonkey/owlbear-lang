
-module(tokstream).

-export([new/0, push_token/2, push_to_chunk/2, end_chunk/1, final/1).


-type deep_tokens() :: [ type:token() | deep_tokens() ].



-record(tok_stream,
	   { stream			= []		:: deep_tokens()
	   , chunk			= []		:: deep_tokens()
	   , error 			= undefined
	   }).


%%%%% ------------------------------------------------------- %%%%%


new() -> #tok_stream{}.


%%%%% ------------------------------------------------------- %%%%%


push_token(#tok_stream{ chunk = [] }, Token) ->
	ok;

push_token(#tok_stream{}, Token) ->
	ok.


%%%%% ------------------------------------------------------- %%%%%


push_to_chunk(#tok_stream{}, Token) ->
	ok.


%%%%% ------------------------------------------------------- %%%%%


end_chunk(#tok_stream{}) ->
	ok.
	


%%%%% ------------------------------------------------------- %%%%%


% final()

