import re

from spark_parser import GenericASTBuilder
from spark_parser.ast import AST
from owlbear.scanner import OwlbearScanner

DEFAULT_DEBUG = {
    "rules": True,
    "transition": True,
    "reduce": True,
    "errorstack": "full",
    "context": True,
    "dups": True,
}

VERBOSE_DEBUG = {
    "rules": False,
    "transition": False,
    "reduce": False,
    "errorstack": "full",
    "context": True,
    "dups": True,
}


class OwlbearParser(GenericASTBuilder):
    def __init__(self, start="module", debug=DEFAULT_DEBUG):
        super().__init__(AST, start, debug=debug)

    def error(self, tokens, index):
        start = index - 2 if index - 2 >= 0 else 0
        context = [tokens[i].highlight() for i in range(start, index + 1)]

        print("\n".join(context))

        super().error(tokens, index)

    def p_forms(self, args):
        """
        module ::= imports
        module ::= forms
        module ::= imports forms

        forms ::= form
        forms ::= form forms

        form ::= TYPE NAME
        form ::= statement

        """

    def p_imports(self, args):
        """
        imports ::= import
        imports ::= imports import

        import ::= IMPORT fq_name
        import ::= IMPORT fq_name AS NAME
        """

    def p_expression(self, args):
        """
        expression ::= expr_or

        expr_or ::= expr_and
        expr_or ::= expr_or OR expr_and

        expr_and ::= expr_cmp
        expr_and ::= expr_cmp AND expr_and

        expr_cmp ::= expr_list
        expr_cmp ::= expr_list CMPEQ expr_list
        expr_cmp ::= expr_list CMPNOTEQ expr_list
        expr_cmp ::= expr_list CMPLTEQ expr_list
        expr_cmp ::= expr_list CMPGTEQ expr_list
        expr_cmp ::= expr_list CMPLT expr_list
        expr_cmp ::= expr_list CMPGT expr_list

        expr_list ::= expr_sum
        expr_list ::= expr_sum LSTADD expr_list
        expr_list ::= expr_sum LSTSUB expr_list

        expr_sum ::= expr_mult
        expr_sum ::= expr_mult PLUS expr_sum
        expr_sum ::= expr_mult MINUS expr_sum
        expr_sum ::= expr_mult BOR expr_sum
        expr_sum ::= expr_mult BSL expr_sum
        expr_sum ::= expr_mult BSR expr_sum

        expr_mult ::= expr_prefix
        expr_mult ::= expr_prefix STAR expr_mult
        expr_mult ::= expr_prefix DIV expr_mult
        expr_mult ::= expr_prefix PERCENT expr_mult
        expr_mult ::= expr_prefix BAND expr_mult

        expr_prefix ::= expr_call
        expr_prefix ::= MINUS expr_call
        expr_prefix ::= PLUS expr_call
        expr_prefix ::= NOT expr_call
        expr_prefix ::= BNOT expr_call

        expr_call ::= expr_leaf
        expr_call ::= NAME LPAREN RPAREN
        expr_call ::= expression DOT NAME LPAREN RPAREN

        expr_leaf ::= LPAREN expression RPAREN
        expr_leaf ::= literal
        expr_leaf ::= ERROR
        expr_leaf ::= NAME
        expr_leaf ::= expression DOT NAME
        """

    def p_pattern(self, args):
        """
        pattern ::= NAME

        """

    def p_statement(self, args):
        """
        block_or_expression ::= expression
        block_or_expression ::= expression SEMI
        block_or_expression ::= block_statement

        statement ::= LET pattern EQUAL block_or_expression
        statement ::= LET QMARK pattern EQUAL block_or_expression
        statement ::= block_statement

        statement_body ::= expression
        statement_body ::= statement_list
        statement_body ::= statement_list expression

        statement_list ::= statement
        statement_list ::= statement statement_list

        block_statement ::= BEGIN statement_body END
        block_statement ::= TRY statement_body CATCH statement_body END
        block_statement ::= TRY statement_body CATCH statement_body FINALLY statement_body END

        block_statement ::= COND cond_clause_body END

        cond_clause_body ::= cond_clause
        cond_clause_body ::= cond_otherwise
        cond_clause_body ::= cond_clause cond_clause_body

        cond_clause ::= expression ARROW block_or_expression

        cond_otherwise ::= OTHERWISE ARROW block_or_expression
        """

    def p_literal(self, args):
        """
        literal ::= NUMBER
        literal ::= STRING
        literal ::= HASH NAME
        literal ::= HASH NAME DOT NAME
        """

    def p_misc(self, args):
        """
        fq_name ::= NAME
        fq_name ::= fq_name DOT NAME
        """
