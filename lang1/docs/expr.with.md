

X = with
        {ok, FileContents}      <- file:read_file(Filename),
        
        Data = binary_to_list(FileContents),
        
        {ok, Tokens, _}         <- scanner:Scan(Data, {1,1}),
        {ok, FilteredTokens}    <- filter_tokens(Tokens),
        {ok, Tree}              <- parser:parse(FilteredTokens),
        
        evaluate(Tree)
    end

    
This works because with will only keep chaining if the value matches
the pattern on the left. If not then the chain is aborted and the
first non-matching result is returned.


with
    [A]Pattern1 <- [B]expr1,
    [C]Pattern2 <- [D]expr2,
    [E]expr3,
    [F]Pattern3 <- [G]expr4
[H]end


-file(File, B.loc).
case expr1 of
-file(File, A.loc).
    Pattern1 = P1 ->
-file(File, D.loc).
        case expr2 of
-file(File, C.loc).
            Pattern2 = P2 ->
-file(File, E.loc).
                expr3,
-file(File, G.loc).
                case expr4 of   % this last case could be optimized out leaving just `expr4`
                                % but that would remove Pattern3 which could be using vars from
                                % previous exprs, result in unused var warnings
-file(File, F.loc).
                    Pattern3 = P3 -> P3
                ;   X@F -> X@F
                end
        ;   X@C -> X@C
        end
;   X@A -> X@A
-file(File, H.loc).
end

