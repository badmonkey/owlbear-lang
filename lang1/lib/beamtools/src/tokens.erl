
-module(tokens).

-export([ from_term/1, from_expr/1, from_expr/2, to_code/1
        , set_location/2, file_attribute/2
        , make_embed/2, get_embed_data/2, is_embed/2
        , get_type/1, get_location/1, get_value/1, has_value/1
        , get_line/1
        , change_type/2 ]).


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
    [];         %% @todo special value for tokstream?? make a token with no location?!
    
make_embed(Tag, Tokens)
        when is_atom(Tag), is_list(Tokens) ->    
    {Tag, get_location( hd(Tokens) ), {embed, Tokens}};
    
make_embed(Tag, Token)
        when is_atom(Tag), is_tuple(Token) ->    
    {Tag, get_location(Token), {embed, Token}}.



-spec get_embed_data( atom(), type:token() ) -> undefined | type:token() | type:tokens().
    
get_embed_data(Tag, {Tag, _, {embed, Data}}) -> Data;
get_embed_data(_, _T) -> undefined.  %% @todo return the token _T ?



-spec is_embed( atom(), type:token() ) -> boolean().
    
is_embed(any, {_, _, {embed, _}})   -> true;
is_embed(Tag, {Tag, _, {embed, _}}) -> true;
is_embed(_, _)                      -> false.


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


-spec change_type( atom(), type:token() ) -> type:token().

change_type(NewType, {_, Loc})      -> {NewType, Loc};
change_type(NewType, {_, Loc, Val}) -> {NewType, Loc, Val}.


%%%%% ------------------------------------------------------- %%%%%


-spec get_line( type:location() ) -> type:ordinal().

get_line({L, _})                -> L;
get_line(L) when is_integer(L)  -> L.


%%%%% ------------------------------------------------------- %%%%%

prefix(S, {L, C}, {LCur, CCur}) ->
    MoreLines = L - LCur,
    MoreSpaces = C - CCur,
    case MoreLines of
        0   ->
            case MoreSpaces of
                0   -> S
            ;   1   -> S
            ;   _   -> string:chars($\s, 1, S)
            end
            
    ;   _ when MoreLines < 3  ->
            case C of
                1   -> string:chars($\n, MoreLines, S)
            ;   _   -> string:chars($\n, MoreLines, string:chars($\s, C, S))
            end
            
    ;   _   ->
            case C of
                1   -> string:chars($\n, 2, S)
            ;   _   -> string:chars($\n, 2, string:chars($\s, C, S))
            end
    end.
    

to_code(Tokens) ->
    {_, Tmp} =  lists:foldl(
                    fun({dot, Loc}, {L, Accum})      ->
                            {Loc, [ io_lib:format(prefix(".", Loc, L), []) | Accum ]}
                        
                    ;  ({Type, Loc}, {L, Accum})        ->
                            {Loc, [ io_lib:format(prefix("~s", Loc, L), [erlang:atom_to_list(Type)]) | Accum ]}
                    
                    ;  ({Type, Loc, Data}, {L, Accum})
                                when Type =:= atom orelse Type =:= var ->
                            {Loc, [ io_lib:format(prefix("~s", Loc, L), [erlang:atom_to_list(Data)]) | Accum ]}
                            
                    ;  ({_, Loc, Data}, {L, Accum})   ->
                            {Loc, [ io_lib:format(prefix("~p", Loc, L), [Data]) | Accum ]}

                    end, {{0,0}, []}, Tokens),
    lists:flatten( lists:reverse(Tmp) ).
