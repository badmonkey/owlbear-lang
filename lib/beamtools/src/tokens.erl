
-module(tokens).

-export([ from_term/1, from_expr/1, from_expr/2
        , set_location/2, file_attribute/2
        , make_embed/2, get_embed_data/2, is_embed/2
		, get_type/1, get_location/1, get_value/1, has_value/1]).


%%%%% ------------------------------------------------------- %%%%%


from_term(Data) ->
    AbsTerm = erl_parse:abstract(Data),
    erl_parse:tokens(AbsTerm).


%%%%% ------------------------------------------------------- %%%%%


from_expr(Expr) -> from_expr(Expr, []).

from_expr(Expr, Params) ->
    {ok, Tok, _} = erl_scan:string( io_lib:format(Expr, Params) ),
    {ok, ParTree} = erl_parse:parse_exprs(Tok),
    {value, Value, _} = erl_eval:exprs(ParTree, []),
    from_term(Value).
     

%%%%% ------------------------------------------------------- %%%%%


-spec set_location( type:location(), type:token() | type:tokens() ) -> type:tokens().

set_location(Location, {Category, _}) ->
    {Category, Location};
    
set_location(Location, {Category, _, Data}) ->
    {Category, Location, Data};
    
    
set_location(Location, Tokens) when is_list(Tokens) ->
    [ set_location(Location, X) || X <- Tokens ].

    
%set_location(Location, {Tag, _, {embed, Data} }) ->
%    {Tag, Location, {embed, set_location(Location, Data)}}.


%%%%% ------------------------------------------------------- %%%%%

    
file_attribute(FileName, {Line,_} = Loc) ->
    [ {'-', Loc}
    , {atom, Loc, 'file'}
    , {'(', Loc}
    , {string, Loc, FileName}
    , {',', Loc}
    , {integer, Loc, Line}
    , {')', Loc}
    , {dot, Loc}
	];

file_attribute(FileName, Line) when is_integer(Line) ->
	file_attribute(FileName, {Line, 1}).



%%%%% ------------------------------------------------------- %%%%%


-spec make_embed( atom(), type:token() | type:tokens() ) -> type:token().

make_embed(_, []) ->
    [];
    
make_embed(Tag, Chunk)
        when is_atom(Tag), is_list(Chunk) ->    
    Data = lists:reverse(Chunk),
    {Tag, element(2, hd(Data)), {embed, Data}};
    
make_embed(Tag, Data)
        when is_atom(Tag), is_tuple(Data), tuple_size(Data) =:= 3 ->    
    {Tag, element(2, Data), {embed, Data}}.



-spec get_embed_data( atom(), type:token() ) -> type:token() | type:tokens().
    
get_embed_data(Tag, {Tag, _, {embed, Data}}) -> Data;
get_embed_data(_, _) -> undefined.



-spec is_embed( atom(), type:token() ) -> boolean().
    
is_embed(any, {_, _, {embed, _}}) 	-> true;
is_embed(Tag, {Tag, _, {embed, _}})	-> true;
is_embed(_, _) 						-> false.


%%%%% ------------------------------------------------------- %%%%%


-spec get_type( type:token() ) -> atom().

get_type({Tag, _}) -> Tag;
get_type({Tag, _, _ }) -> Tag;
get_type(_) -> undefined.


-spec get_location( type:token() ) -> type:location().

get_location({_, Loc}) -> Loc;
get_location({_, Loc, _}) -> Loc;
get_location(_) -> undefined.
	

-spec get_value( type:token() ) -> type:symbol().

get_value({_, _, Val}) -> Val;
get_value(_) -> undefined.


-spec has_value( type:token() ) -> boolean().

has_value({_, _, _}) -> true;
has_value(_) -> false.


