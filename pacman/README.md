

primitive recursion only
must terminate
no memory allocation
no user callbacks to avoid recursion

https://github.com/google/wuffs/tree/main/doc/note
https://en.wikipedia.org/wiki/LOOP_(programming_language)
https://www.cis.upenn.edu/~cis194/spring13/lectures/03-rec-poly.html


forloop ::= "for" IDENT [: TYPEXPR] "in" rnge_expr statementlist "end"

rnge_expr ::= range(e)
rnge_expr ::= e1 .. e2


statement ::= error
statement ::= forloop
statement ::= guard
statement ::= ifthen
statement ::= switch
statement ::= trycatch
statement ::= return



interface Container
    uint32 length();
    T next();
end


public
IDENT(ARGS) -> TYPE
end

reduce(init : T, data : array(T), op : (T,T)->T) -> T
    var accum : T = init
    for x in data
        accum = op(accum, x)
    end
    return accum
end
