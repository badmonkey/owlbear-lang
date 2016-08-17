

ifguard
    G1 -> body;
...
    GN -> body;
else
    else_expr
end


if
    G1 -> body;
...
    GN -> body;
    true -> else_expr
end


---------------------------------------------------------------


if
;	{ok, Name}  <- get_name(Obj)
;   {ok, R}     <- remote:get_remote_name(Obj)
;   {ok, Name}  <- database:make_new_name(Obj)
;	else -> {error, something}
end


case E1 of
;   P1 = X@1 -> X@1
else case
...
else case EN of
;   PN - X@1 -> X@N
else
    else_expr
end


case get_name(Obj) of
;	{ok, Name} = X@1 -> X@1
;	_ ->
		case remote:get_remote_name(Obj)
		;	{ok, R} = X@2 -> X@2
		;	_ ->
				case database:make_new_name(Obj) of
				;	{ok, Name} = X@3 -> X@3
				;	_ -> {error, something}
				end
		end
end


---------------------------------------------------------------


ifcond
;   Name = get_name(Obj)                            -> Name                 % false and undefined are 'false' everything else is 'true'
;   {remote, Remote} = remote:get_remote_name(Obj)  -> {global, Remote}     % what happens to bad_match errors?
;   Name = database:make_new_name(Obj)              -> {local, Name}        % variables shouldn't be bound across ifcond clauses
else
    {error, something}
end

ifcond
;   TooHot(S, Max)  -> MakeColder()
;   TooCold(S, Min) -> MakeHotter()
;   else            -> LeaveHeaterAlone()
end


ifcond
    E1 -> body;
...
    EN -> body;
else
    else_expr
end


case E1 of
    X@1 when X@1 =:= false orelse X@1 =:= undefined ->
        case E2 of
            X@2 when X@2 =:= false orelse X@2 =:= undefined -> else_expr
        ;   _ -> body
        end
;   _ -> body
        
end

