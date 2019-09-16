
% Alternative Erlang preprocessor

Nonterminals
    Module
    Elements
    ElementSeq
    Element

    Chunk
    Identifier
    
    ClientExpr
    ExprList
    Expr

    Include
    Include_lib

    ApplyMacro
    ApplyMacroString
    MacroArgs

    IfDef
    IfNDef
    Else
    EndIf
    Define
    UnDefine

    UserDirective

    IfBlock
    IfNBlock
    .


Terminals
    identifier string
    include_keyword include_lib_keyword
    define_keyword undef_keyword
    ifdef_keyword ifndef_keyword else_keyword endif_keyword  
    '?'  '-'  '('  ')'  ','
    'beamtools$CHUNK'  'beamtools$RAW'
    dot
    .


Rootsymbol Module.


Module -> Elements                                  : '$1'.

Elements -> '$empty'                                : {sequence, []}.
Elements -> ElementSeq                              : {sequence, '$1'}.

ElementSeq -> Element                               : ['$1'].
ElementSeq -> Element ElementSeq                    : ['$1' | '$2'].

Element -> Chunk                                    : '$1'.
Element -> Define                                   : '$1'.
Element -> UnDefine                                 : '$1'.
Element -> IfBlock                                  : '$1'.
Element -> IfNBlock                                 : '$1'.
Element -> UserDirective                            : '$1'.
Element -> Include                                  : '$1'.
Element -> Include_lib                              : '$1'.
     
    
Chunk -> 'beamtools$CHUNK'                          : {write, unchunk('$1')}.
Chunk -> ApplyMacro                                 : '$1'.

Identifier -> identifier                            : tokens:get_value('$1').


ClientExpr -> ExprList                              : {expr, '$1'}.

ExprList -> Expr                                    : ['$1'].
ExprList -> Expr ExprList                           : ['$1' | '$2'].

Expr -> 'beamtools$CHUNK'                           : {tokens, unchunk('$1')}.
Expr -> ApplyMacro                                  : '$1'.
Expr -> ApplyMacroString                            : '$1'.


Define -> define_keyword '(' Identifier ')' dot                                    	: {define}.
Define -> define_keyword '(' Identifier ',' ClientExpr ')' dot                     	: {define}.
Define -> define_keyword '(' Identifier '(' ')' ',' ClientExpr ')' dot             	: {define}.
Define -> define_keyword '(' Identifier '(' MacroArgs ')' ',' ClientExpr ')' dot 	: {define}.

UnDefine -> undef_keyword '(' Identifier ')' dot    : {undef, '$3'}.


IfBlock  -> IfDef Elements Else Elements EndIf      : {test_and_write, '$1', '$2', '$4'}.
IfBlock  -> IfDef Elements EndIf                    : {test_and_write, '$1', '$2', undefined}.

IfNBlock -> IfNDef Elements Else Elements EndIf     : {test_and_write, '$1', '$4', '$2'}.
IfNBlock -> IfNDef Elements EndIf                   : {test_and_write, '$1', undefined, '$2'}.


IfDef  -> ifdef_keyword '(' Identifier ')' dot      : '$3'.
IfNDef -> ifndef_keyword '(' Identifier ')' dot     : '$3'.
Else   -> else_keyword dot.
EndIf  -> endif_keyword dot.


Include -> include_keyword '(' string ')' dot           : {include, file, '$3'}.
Include_lib -> include_lib_keyword '(' string ')' dot   : {include, resolve, '$3'}.


UserDirective -> '-' Identifier 'beamtools$RAW' '(' ')' dot            	: {user_directive, '$2', ['$1', unraw('$3'), '$4', '$5', '$6']}.
UserDirective -> '-' Identifier 'beamtools$RAW' '(' ClientExpr ')' dot	: {user_directive, '$2', '$5', ['$1', unraw('$3'), '$4'], ['$6', '$7']}.


ApplyMacro -> '?' Identifier                        : {expand_macro, '$2'}.
ApplyMacro -> '?' Identifier '(' ClientExpr ')'     : {expand_macro, '$2', '$4'}.

ApplyMacroString -> '?' '?' Identifier              : {expand_macro_string, '$3'}.


MacroArgs -> Identifier                             : ['$1'].
MacroArgs -> Identifier ',' MacroArgs               : ['$1' | '$3'].



Erlang code.
%%%%% ------------------------------------------------------- %%%%%


-export([evaluate/2]).


%%%%% ------------------------------------------------------- %%%%%


evaluate(Tree, State) ->
    lists:flatten( eval(Tree, State) ).

    
%%%%% ------------------------------------------------------- %%%%%
    

eval(undefined, State) ->
    [];
    
eval({sequence, Elements}, State) ->
    [ eval(X, State) || X <- Elements ];
    
eval({write, Tokens}, State) ->
    Tokens;

    
eval({test_and_write, Macro, True, False}, State) ->
    case preprocessor:has_macro(Macro, State) of
        true    -> eval(True, State)
    ;   false   -> eval(False, State)
    end;

    
eval({user_directive, Directive, Original}, State) ->
    Original;
        
eval({user_directive, Directive, Expr, Head, Tail}, State) ->
    Head ++ Tail;

    
eval({expand_macro, Name}, State) ->
    [];
    
eval({expand_macro, Name, ParamChunk}, State) ->
    [];

eval({expand_macro_string, Name}, State) ->
    [];
    
    
eval(_, _) ->
    {error, unknown_pp_cmd}.
    

%%%%% ------------------------------------------------------- %%%%%


unraw(X) ->
    tokens:get_embed_data('beamtools$RAW', X).
    
unchunk(X) ->
    tokens:get_embed_data('beamtools$CHUNK', X).


