
-module(type).

-export_type([ natural/0, cardinal/0, ordinal/0
             , line/0, column/0, location/0
             , symbol/0, token/0, tokens/0, error_info/0, token_return/0
             , scanner/0, reserved_predicate/0 ]).

             
%%%%% ------------------------------------------------------- %%%%%


-type natural() :: non_neg_integer().
-type cardinal() :: non_neg_integer().  % quantity of things numbers ie 0, 1, ...
-type ordinal() :: pos_integer().       % order of things numbers 1st, 2nd, 3rd


-type line() :: ordinal().
-type column() :: ordinal().
-type location() :: line() | {line(), column()}.

-type symbol() :: atom() | float() | integer() | string().
-type token() :: {atom(), location()}
               | {atom(), location(), symbol()}
               | {atom(), location(), {embed, [token()]} }.
-type tokens() :: [token()].
               
-type error_info() :: {location(), module(), term()}.
               
-type token_return() :: {ok, Tokens :: tokens(), EndLocation :: location()}
                      | {error, ErrorInfo :: error_info(), ErrorLocation :: location()}.
                      
-type scanner() :: fun( ( string(), location() ) -> token_return() ).
-type reserved_predicate() :: fun( ( atom() ) -> boolean() ).


