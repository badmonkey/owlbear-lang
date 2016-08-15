

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
;   {ok, Name}  <- get_name(Obj)                -> {local, Name}
;   {ok, R}     <- remote:get_remote_name(Obj)  -> {global, R}
;   {ok, Name}  <- database:make_new_name(Obj)  -> {global, Name}
else
    {error, something}
end



if
    P1 <- E1 -> body;
...
    PN <- EN -> body;
else
    else_expr
end


case E1 of
;   P1 -> body
...
else case EN of
;   PN -> body
else
    else_expr
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
