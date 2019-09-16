
-module(stdforms).

-export([from_string/1, from_string/2, from_string_ex/2]).

-export_type([ form/0
             , substitutions/0
             ]).


%%%%% ------------------------------------------------------- %%%%%


-type form() :: erl_parse:abstract_form().
-type substitutions() :: #{ string() => form() }.


%%%%% ------------------------------------------------------- %%%%%


%-spec first_location( form() ) -> location().

%first_location(Form) -> 1.

%last_location( form() ) -> location()
%line_count( form() ) -> base:cardinal()


%%%%% ------------------------------------------------------- %%%%%

% {ok, AbsForm} | {error, ErrorInfo}
% {ok, ExprList} | {error, ErrorInfo}
% error_info() = {erl_anno:line(), module(), error_description()}


from_string(Str) -> from_string(Str, {1, 1}).


-spec from_string( string(), type:location() ) -> form().

from_string(Str, Loc) ->
    {AbsForm, _} = from_string_ex(Str, Loc),
    AbsForm.

    
-spec from_string_ex( string(), type:location() ) -> { form(), type:location() }.

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
    

-spec reset_locations( form(), type:location() | fun( ( type:location() ) -> type:location() ) ) -> form().

reset_locations(Form, X)
        when is_integer(X) ->
    reset_locations(Form, fun(_) -> {X, 1} end);
    
reset_locations(Form, {L, C})
        when is_integer(L), is_integer(C) ->
    reset_locations(Form, fun(_) -> {L, C} end);

reset_locations(Form, _Mutate) ->
    Form.

    
%-spec reset_locations( form(), fun( ( type:location(), term() ) -> {type:location(), term()} ), term() ) -> form().

%reset_locations(Form, _Mutate) -> Form.


%%%%% ------------------------------------------------------- %%%%%


%relabel_variables( form(), #{ string() => string() } ) -> form()
%relabel_variables( form(), fun( ({var, Loc, Name}, State) -> string() ), State ) -> form()

%expand_function( form(), [term()] ) -> form()
% 

%replace_variable( form(), substitutions() ) -> form().


%%%%% ------------------------------------------------------- %%%%%



%%% AST members %%%

% []    only the empty list
% atom()

%{atom, LINE, L}
%{char, LINE, L}
%{float, LINE, L}
%{integer, LINE, L}
%{string, LINE, string()}

%{attribute, LINE, export, LIST}
%{attribute, LINE, import, TUPLE}
%{attribute, LINE, module, Mod}
%{attribute, LINE, file, TUPLE}
%{attribute, LINE, spec/callback, {TUPLE, list(AST)}}
%{attribute, LINE, record, {Name, list(AST)}}
%{attribute, LINE, type/opaque, {Name, AST, list(AST)}}
%{attribute, LINE, A, T}    for -A(T)
%{ann_type, LINE, [AST, AST]}

%{bin, LINE, list(bin_element)}
%{bin_element, LINE, AST, AST|default, AST|default}
%{bc, LINE, AST, list(AST)}
%{block, LINE, AST}.
%{b_generate, LINE, AST, AST}

%{cons, LINE, AST, AST}
%{'case', LINE, AST, list(AST)}
%{'catch', LINE, AST}
%{call, LINE, AST, list(AST)}
%{call, LINE, {remote, LINE, AST, AST}, list(AST)}
%{clause, LINE, [AST], AST, AST}
%{clause, LINE, [{atom(), AST?, _}], AST, AST}
%{clause, LINE, AST, AST, AST}

%{function, LINE, Name, Arity, list(AST)}
%{'fun', LINE, {function, Name, Arity}}
%{'fun', LINE, {function, AST, AST, AST}}
%{'fun', LINE, {clauses, list(AST)}}

%{generate, LINE, AST, AST}

%{'if', LINE, list(AST)}

%{lc, LINE, AST, list(AST)}

%{match, LINE, AST, AST}
%{map, LINE, list(AST)}
%{map, LINE, AST, list(AST)}
%{map_field_assoc, LINE, AST, AST}
%{map_field_exact, LINE, AST, AST}

%{named_fun, LINE, Name, list(AST)}
%{nil, LINE}

%{op, LINE, Op, AST, AST}
%{op, LINE, Op, AST}.

%{record_index, LINE, Name, Rep(Field)}.
%{record, LINE, Name, list(record_field/4)}
%{record, LINE, AST, Name, list(record_field/4)}
%{record_field, LINE, AST}
%{record_field, LINE, AST, AST}
%{record_field, LINE, AST, Name, AST}
%{remote_type, LINE, [AST, AST, list(AST)]}
%{'receive', LINE, list(AST)}
%{'receive', LINE, list(AST), AST, AST}

%{type, LINE, N, list(AST)}
%{type, LINE, record, list(AST)}
%{type, LINE, tuple, list(AST)|any}
%{type, LINE, union, list(AST)}
%{type, LINE, bounded_fun, list(AST)}
%{type, LINE, 'fun', []}
%{type, LINE, 'fun', [{type, LINE, any}, AST]}
%{type, LINE, 'fun', [{type, LINE, product, list(AST)}, AST]}
%{type, LINE, map_field_assoc, [AST, AST]}
%{type, LINE, map_field_exact, [AST, AST]}
%{type, LINE, field_type, [AST, AST]}
%{type, LINE, binary, [AST, AST]}
%{type, LINE, nil, []}
%{type, LINE, range, [AST, AST]}
%{type, LINE, map, list(AST)|any}
%{typed_record_field, {record_field, LINE, AST}, AST}
%{typed_record_field, {record_field, LINE, AST, AST}, AST}
%{tuple, LINE, list(AST)}.
%{'try', LINE, AST, list(AST), list(AST), AST}

%{user_type, LINE, N, list(AST)}

%{var, LINE, Name}






