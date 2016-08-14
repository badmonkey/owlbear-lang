

case E of
    P1 -> body;
...
    PN -> body;
else
    else_expr
end

case E of
    P1 -> body;
...
    PN -> body;
    else -> else_expr
end


---------------------------------------------------------------


case E1 of
    E1P1 -> body;
...
    E1PN -> body;
else case E2 of
    E2P1 -> body;
...
    E2PN -> body;
else case E3 of
    P5 -> body;
...
    P6 -> body;
else
    else_expr
end



case E1 of
    E1P1    -> body;
...    
    E1PN    -> body;
    _ = &1  ->
        case E2 of
            E2P1    -> body;
        ...
            E2PN    -> body;
            _ = &2  ->
                case E3 of
                    E3P1    -> body;
                ...
                    E3PN    -> body;
                    _ = &3  -> else_expr
                end
        end
end

