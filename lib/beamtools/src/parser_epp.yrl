% Alternative Erlang preprocessor

Nonterminals
    File
	Elements
	ElementSeq

	Chunks
	Chunk
	ChunkExpr
	ChunkExprItem

	Include
	Include_lib

    ApplyMacro
    ApplyMacroString
    MacroName
	MacroArgs

	IfDef
	IfNDef
	Else
	EndIf
	Define

	UserDirective

	IfBlock
	IfNBlock
	ElseBlock

	.


% Terminals taken from erl_parse.yrl
Terminals
	atom var string
	ifdef_pp ifndef_pp else_pp endif_pp define_pp include_pp include_lib_pp
    '?'  '-'  '('  ')'  ','
	'beamtools$CHUNK'
	'beamtools$DIRECTIVE'
    dot
	.


Rootsymbol File.

Unary 200 '('.


File -> Elements 						: '$1' ++ [{eof, 0}].


Elements -> ElementSeq					: {sequence, '$1'}.

ElementSeq -> '$empty'					: [].
ElementSeq -> ElementSeq Chunks			: '$1' ++ ['$2'].
ElementSeq -> ElementSeq IfBlock		: '$1' ++ ['$2'].
ElementSeq -> ElementSeq IfNBlock		: '$1' ++ ['$2'].
ElementSeq -> ElementSeq UserDirective	: '$1' ++ ['$2'].
ElementSeq -> ElementSeq Define			: '$1' ++ ['$2'].
ElementSeq -> ElementSeq Include		: '$1' ++ ['$2'].
ElementSeq -> ElementSeq Include_lib	: '$1' ++ ['$2'].
	 
	
Chunks -> Chunk							: ['$1'].
Chunks -> Chunks Chunk					: '$1' ++ ['$2'].

Chunk -> 'beamtools$CHUNK'				: '$1'.
Chunk -> ApplyMacro						: '$1'.

ChunkExpr -> ChunkExprItem				: ['$1'].
ChunkExpr -> ChunkExpr ChunkExprItem	: '$1' ++ ['$2'].

ChunkExprItem -> Chunk					: '$1'.
ChunkExprItem -> ApplyMacroString		: '$1'.
	


IfDef  -> '-' ifdef_pp '(' MacroName ')' dot.
IfNDef -> '-' ifndef_pp '(' MacroName ')' dot.
Else   -> '-' else_pp dot.
EndIf  -> '-' endif_pp dot.

Define -> '-' define_pp '(' MacroName ')' dot.
Define -> '-' define_pp '(' MacroName ',' ChunkExpr ')' dot.
Define -> '-' define_pp '(' MacroName '(' ')' ',' ChunkExpr ')' dot.
Define -> '-' define_pp '(' MacroName '(' MacroArgs ')' ',' ChunkExpr ')' dot.


IfBlock  -> IfDef Elements ElseBlock		: {pp_ifdef, '$1', '$2', '$3'}.
IfNBlock -> IfNDef Elements ElseBlock		: {pp_ifndef, '$1', '$2', '$3'}.

ElseBlock -> Else Elements EndIf			: '$2'.
ElseBlock -> EndIf							: {sequence, []}.


Include -> '-' include_pp '(' string ')' dot.
Include_lib -> '-' include_lib_pp '(' string ')' dot.


UserDirective -> '-' 'beamtools$DIRECTIVE' '(' ')' dot				: {user_directive, '$2', undefined}.
UserDirective -> '-' 'beamtools$DIRECTIVE' '(' ChunkExpr ')' dot	: {user_directive, '$2', '$4'}.


ApplyMacro -> '?' MacroName 					: {'expand_macro', '$2'}.
ApplyMacro -> '?' MacroName '(' ChunkExpr ')'	: {'expand_macro', '$2', '$4'}.

ApplyMacroString -> '?' '?' var 				: {'expand_macro_string', '$3'}.


MacroName -> atom 								: '$1'.
MacroName -> var 								: '$1'.
	 
MacroArgs -> var 								: ['$1'].
MacroArgs -> MacroArgs ',' var 					: '$1' ++ ['$3'].



