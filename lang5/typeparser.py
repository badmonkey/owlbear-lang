import re
from spark_parser import GenericASTBuilder
from spark_parser.ast import AST
from spark_parser.scanner import GenericScanner, GenericToken


DEFAULT_DEBUG = {
    "rules": True,
    "transition": True,
    "reduce": True,
    "errorstack": "full",
    "context": True,
    "dups": True,
}


class TypeParser(GenericASTBuilder):
    def __init__(self, start="typeform", debug=DEFAULT_DEBUG):
        super().__init__(AST, start, debug=debug)

    def p_form(self, args):
        """
        finaltype ::= typexpr
        finaltype ::= typexpr DBLCOLON IDENT+

        typeform ::= TYPE IDENT IS finaltype
        typeform ::= TYPE IDENT LPAREN typeparamlist RPAREN IS finaltype
        typeform ::= PRIMITIVE typenamelist

        """

    def p_expression(self, args):
        """
        typeunionlist ::= typexpr
        typeunionlist ::= typeunionlist OR typexpr

        typeprodlist ::= typexpr
        typeprodlist ::= typeprodlist AND typexpr

        typexpr ::= IDENT
        typexpr ::= IDENT LPAREN typearglist RPAREN
        typexpr ::= LPAREN typeunionlist RPAREN
        typexpr ::= LPAREN typexpr AND typeprodlist RPAREN
        typexpr ::= LBRACE typelist RBRACE
        typexpr ::= LBRACE typexpr COLON typexpr RBRACE
        typexpr ::= LBRACKET typexpr RBRACKET
        """

    def p_misc(self, args):
        """
        typelist ::= typexpr
        typelist ::= typelist COMMA typexpr

        typearg ::= typexpr
        typearg ::= LITERAL

        typearglist ::= typearg
        typearglist ::= typearglist COMMA typearg

        typeparam ::= IDENT
        typeparam ::= typexpr COLON IDENT

        typeparamlist ::= typeparam
        typeparamlist ::= typeparamlist COMMA typeparam

        typenamelist ::= IDENT
        typenamelist ::= typenamelist COMMA IDENT
        """



RESERVED_WORDS = re.split("\s+", """type is primitive""")


BRACKET2NAME = {
    '(': 'LPAREN',   ')': 'RPAREN',
    '{': 'LBRACE',   '}': 'RBRACE',
    '[': 'LBRACKET', ']': 'RBRACKET',
    }

SYMBOL2NAME = {
    '@': 'AT',    '`': 'BACKTICK',
    ':': 'COLON', ',': 'COMMA',
    '.': 'DOT',
    '|': 'OR',    '&': 'AND',
    }

ENDMARKER = r''


class TypeScanner(GenericScanner):
    """A simple integer expression Parser.
    Note: function parse() comes from GenericASTBuilder
    """

    def __init__(self):
        GenericScanner.__init__(self)

    def tokenize(self, input):
        self.rv = []
        GenericScanner.tokenize(self, input)
        return self.rv

    def add_token(self, name, s):
        t = GenericToken(kind=name, attr=s)
        self.rv.append(t)

    def t_endmarker(self, s):
        """"""
        self.add_token('ENDMARKER', s)

    def t_whitespace(self, s):
        r' \s+ '
        pass

    def t_nl(self, s):
        r'\n'
        pass

    def t_paren(self, s):
        r'[(){}[\]]'
        self.add_token(BRACKET2NAME[s], s)

    def t_annotation(self, s):
        r'[:][:]'
        self.add_token(DBLCOLON, s)

    def t_symbol(self, s):
        r'[@:,.`|&]'
        self.add_token(SYMBOL2NAME[s], s)

    def t_name(self, s):
        r'[A-Za-z_][A-Za-z_0-9]*'
        if s in RESERVED_WORDS:
            self.add_token(s.upper(), s)
        else:
            self.add_token('IDENT', s)


scan = TypeScanner()
parser = TypeParser()

# parser.check_grammar()
# parser.check_sets()
# parser.dump_grammar()


tokens = scan.tokenize("""
type Person is { String, Int, [String] }
""" + ENDMARKER)

#type Person is ( String & Int & [(String & [String] | String)] )

# for t in tokens:
#     print(t)


ast = parser.parse(tokens)
print(ast)
