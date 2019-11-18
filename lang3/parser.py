from spark_parser import GenericASTBuilder
from spark_parser.ast import AST

DEFAULT_DEBUG = {
    "rules": True,
    "transition": True,
    "reduce": True,
    "errorstack": "full",
    "context": True,
    "dups": True,
}


class PythonParser(GenericASTBuilder):
    def __init__(self, start="expression", debug=DEFAULT_DEBUG):
        super().__init__(AST, start, debug=debug)

    def p_expression(self, args):
        """
        expression ::= BRA sequence KET

        sequence ::= expression
        sequence ::= sequence SEMI expression

        expression ::= expression SEND expression
        expression ::= RECEIVE
        expression ::= RECEIVE AFTER NUMBER expression

        expression ::= literal
        """

    def p_functions(self, args):
        """
        expression ::= LAMBDA ident_seq COLON expression
        expression ::= identifier BRA KET
        expression ::= identifier BRA param_seq KET

        param_seq ::= expression
        param_seq ::= param_seq COMMA expression
        """

    def p_identifiers(self, args):
        """
        identifier ::= IDENT
        identifier ::= IDENT DOT IDENT

        full_ident ::= IDENT
        full_ident ::= full_ident DOT IDENT

        ident_seq ::= IDENT
        ident_seq ::= ident_seq COMMA IDENT
        """

    def p_literals(self, args):
        """
        literal ::= literal_string
        literal ::= NUMBER

        literal_string ::= STRING
        literal_string ::= literal_string STRING
        """

    def p_let(self, args):
        """
        expression ::= LET let_seq IN sequence
        expression ::= LETREC let_seq IN sequence

        let_seq ::= let_expr
        let_seq ::= let_seq SEMI let_expr

        let_expr ::= IDENT EQUAL expression
        """

    def p_condition(self, args):
        """
        expression ::= COND cond_clause_seq

        cond_clause_seq ::= cond_clause
        cond_clause_seq ::= cond_clause_seq PIPE cond_clause

        cond_clause ::= expression THEN sequence
        """

    def p_imports(self, args):
        """
        expression ::= IMPORT import_seq IN expression
        expression ::= EXPORT ident_seq

        import_seq ::= import_expr
        import_seq ::= import_seq SEMI import_expr

        import_expr ::= IDENT EQUAL full_ident
        """

    def p_match(self, args):
        """
        expression ::= MATCH expression WITH match_clause_seq

        match_clause_seq ::= match_clause
        match_clause_seq ::= match_clause_seq match_clause

        match_clause ::= PIPE pattern THEN sequence

        pattern ::= IDENT
        pattern ::= CONBRA IDENT CONKET
        pattern ::= CONBRA IDENT constructors CONKET

        constructors ::= construct
        constructors ::= constructors construct

        construct ::= literal
        construct ::= IDENT
        construct ::= pattern
        """


parser = PythonParser()

parser.check_grammar()
# parser.check_sets()
# parser.dump_grammar()


def usage():
    """
IMPORT list = std.list;
       string = std.string IN
    LET start = LAMBDA self, x: LET t = list.sort(x)
                                ;   t2 = string.first(t) IN
                                        bamm(t2)
    ;   stop = LAMBDA self: terminate(self) IN
            EXPORT start, stop

COND x == 0 THEN dozero()
|    x > 100 THEN LET a = x + 1 IN print(a)
|    iseven(y) THEN print(y)

LET timeout = LAMBDA: print()
;   val = RECEIVE AFTER 10 timeout()
IN
    MATCH val
    |  [nil] THEN ...
    |  [cons 0 rest] THEN ...
    |  [cons x rest] THEN


MATCH some_list()
|   [nil] THEN ...
|   [cons 1 [cons x rest]] THEN ...


LET map_r = lambda r, f, l: MATCH l
                            | [nil]  THEN nil()
                            | [cons hd rest] THEN cons(f(hd), r(r, f, rest))
;   map = lambda f, l: map_r(map_r, f, l)
IN
    EXPORT map


LET x = 2 IN
    y = foo(x);
    bob(y)


    """
