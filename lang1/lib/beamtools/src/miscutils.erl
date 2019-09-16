
-module(miscutils).

-export([ if_valid_then_update/5 ]).


%%%%% ------------------------------------------------------- %%%%%

-spec if_valid_then_update( atom(), #{ atom() => term() }, fun( ( atom() ) -> boolean() ), integer(), tuple() ) -> tuple().

if_valid_then_update(Key, Map, Predicate, Index, Record) ->
    case maps:get(Key, Map, undefined) of
        undefined   -> Record
    ;   X           ->
            case Predicate(X) of
                false   -> Record
            ;   true    -> setelement(Index, Record, X)
            end
    end.

    
%%%%% ------------------------------------------------------- %%%%%

    