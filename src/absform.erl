
-module(absform).

-export([from_string/1, from_string/2, from_string_ex/2]).

-export_type([ line/0, column/0, location/0
             , form/0
             , substitutions/0
             ]).


%%%%% ------------------------------------------------------- %%%%%


-type line() :: base:ordinal().
-type column() :: base:ordinal().
-type location() :: line() | {line(), column()}.

-type form() :: erl_parse:abstract_form().
-type error_info() :: term().

-type substitutions() :: #{ string() => form() }.


%%%%% ------------------------------------------------------- %%%%%


%-spec after( location(), location() ) -> boolean().

% is Y after X location wise. A Line by itself is taken to mean the first column of that line
%after(X, Y) when is_integer(X), is_integer(Y)   -> X < Y;
%after(X, Y) when is_integer(X)                  -> after({X, 1}, Y);
%after(X, Y) when is_integer(Y)                  -> after(X, {Y, 1});
%after({Xl, Xc}, {Yl, Yc})                       ->
%    case Xl < Yl of
%        true    -> true
%    ;   false   ->
%            case Xl =:= Yl of
%                true    -> Xc < Yc
%            ;   false   -> false
%            end
%    end.


%%%%% ------------------------------------------------------- %%%%%


%-spec first_location( form() ) -> location().

%first_location(Form) -> 1.

%last_location( form() ) -> location()
%line_count( form() ) -> base:cardinal()


%%%%% ------------------------------------------------------- %%%%%


from_string(Str) -> from_string(Str, {1, 1}).


-spec from_string( string(), location() ) -> form().

from_string(Str, Loc) ->
    {AbsForm, _} = from_string_ex(Str, Loc),
    AbsForm.

    
-spec from_string_ex( string(), location() ) -> { form(), location() }.

from_string_ex(Str, Loc) ->
    {ok, Tokens, Location} = erl_scan:string(Str, Loc),
    FirstLoc = erl_scan:location( hd(Tokens) ),
    {ok, AbsForm} = try
                        {ok, _} = erl_parse:parse_form(Tokens)
                    catch
                        _:_ ->
                            {ok, ExForm} = erl_parse:parse_exprs(Tokens),
                            { ok, {block, FirstLoc, ExForm} }
                    end,
    { AbsForm, Location }.
    
    
%%%%% ------------------------------------------------------- %%%%%
    

-spec reset_locations( form(), location() | fun( ( location() ) -> location() ) ) -> form().

reset_locations(Form, X)
        when is_integer(X) ->
    reset_locations(Form, fun(_) -> {X, 1} end);
    
reset_locations(Form, {L, C})
        when is_integer(L), is_integer(C) ->
    reset_locations(Form, fun(_) -> {L, C} end);

reset_locations(Form, _Mutate) -> Form.


%%%%% ------------------------------------------------------- %%%%%


%relabel_variables( form(), #{ string() => string() } ) -> form()
%relabel_variables( form(), fun( ({var, Loc, Name}, State) -> string() ), State ) -> form()

%expand_function( form(), [term()] ) -> form()
% 

%replace_variable( form(), substitutions() ) -> form().


%%%%% ------------------------------------------------------- %%%%%


%{attribute, LINE, export, []}
%{attribute, LINE, import, {}}
%{attribute, LINE, module, Mod}
%{attribute, LINE, file, {}}

%{function, LINE, Name, Arity, []}
%{attribute, LINE, spec/callback, {}}
%{attribute, LINE, record, {}}
%{attribute, LINE, type/opaque, {}}

%{attribute, LINE, A, T} for -A(T)

%{record_field, LINE, Rep(A)}
%{record_field, LINE, Rep(A), Rep(E)}
%{typed_record_field, {record_field, LINE, Rep(A)}, Rep(T)}
%{typed_record_field, {record_field, LINE, Rep(A), Rep(E)}, Rep(T)}

%{atom, LINE, L}
%{char, LINE, L}
%{float, LINE, L}
%{integer, LINE, L}
%{string, LINE, []}



    

