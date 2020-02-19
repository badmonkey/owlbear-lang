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

BASIC_DEBUG = {
    "rules": False,
    "transition": False,
    "reduce": False,
    "errorstack": "full",
    "context": True,
    "dups": True,
}

class TypeParser(GenericASTBuilder):
    def __init__(self, start="forms", debug=DEFAULT_DEBUG):
        super().__init__(AST, start, debug=debug)

    def p_form(self, args):
        """
        forms ::= form
        forms ::= forms form

        form ::= typeform
        form ::= funcdefn


        funcdefn ::= funchead funcparams ARROW typereturn exprlist
        funcdefn ::= funchead funcparams ARROW typereturn exprlist END

        funchead ::= funcname
        funchead ::= PUBLIC funcname

        funcparams ::= LPAREN RPAREN
        funcparams ::= LPAREN funcparamlist RPAREN

        exprlist ::= expr
        exprlist ::= exprlist expr
        exprlist ::= exprlist SEMI expr

        expr ::= EXPR
        expr ::= LET ssalist
        expr ::= RECEIVE
        expr ::= ERROR
        expr ::= TRY exprlist CATCH exprlist
        expr ::= TRY exprlist CATCH exprlist END

        ssalist ::= ssa
        ssalist ::= ssalist ssa

        ssa ::= varname EQUAL expr
        ssa ::= varname COLON typereturn EQUAL expr

        funcparamlist ::= funcparam
        funcparamlist ::= funcparamlist COMMA funcparam

        funcparam ::= varname
        funcparam ::= varname COLON typereturn

        typeform ::= TYPE typename IS typexpr
        typeform ::= TYPE typename LPAREN typeparamlist RPAREN IS typexpr

        typeparamlist ::= typeparam
        typeparamlist ::= typeparamlist COMMA typeparam

        typeparam ::= typename
        typeparam ::= typename SUBTYPE typexpr

        typeform ::= PRIMITIVE typenamelist
        """

    def p_expression(self, args):
        """
        typexpr ::= typename
        typexpr ::= typename LPAREN typearglist RPAREN
        typexpr ::= LPAREN typeunionlist RPAREN
        typexpr ::= LPAREN typexpr AND typeprodlist RPAREN
        typexpr ::= LBRACE typelist RBRACE
        typexpr ::= LBRACE typexpr COLON typexpr RBRACE
        typexpr ::= LBRACKET typexpr RBRACKET

        typeunionlist ::= typexpr
        typeunionlist ::= typeunionlist OR typexpr

        typeprodlist ::= typexpr
        typeprodlist ::= typeprodlist AND typexpr
        """

    def p_trait(self, args):
        """
        typeform ::= TRAIT typename typefunclist
        typeform ::= TRAIT typename typefunclist END

        typefunclist ::= typefunc
        typefunclist ::= typefunclist typefunc

        typefunc ::= funcname LPAREN RPAREN ARROW typereturn
        typefunc ::= funcname LPAREN typelist RPAREN ARROW typereturn
        """

    def p_return(self, args):
        """
        typereturn ::= typexpr
        typereturn ::= tracelist typexpr
        typereturn ::= tracelist SEMI typexpr

        tracelist ::= tracename
        tracelist ::= tracelist tracename

        tracename ::= TRACE
        tracename ::= RECEIVE LPAREN typeunionlist RPAREN
        """

    def p_misc(self, args):
        """
        typelist ::= typexpr
        typelist ::= typelist COMMA typexpr

        typearglist ::= typearg
        typearglist ::= typearglist COMMA typearg

        typearg ::= typexpr
        typearg ::= LITERAL

        typenamelist ::= typename
        typenamelist ::= typenamelist COMMA typename

        typename ::= IDENT

        funcname ::= IDENT
        funcname ::= TRACE

        varname ::= IDENT
        varname ::= TRACE
        """


RESERVED_WORDS = re.split("\s+", """type is primitive or and trait end error let receive try catch public expr""")
TRACE_WORDS = re.split("\s+", """unsafe stackbound pure constant tailcall""")


BRACKET2NAME = {
    "(": "LPAREN",
    ")": "RPAREN",
    "{": "LBRACE",
    "}": "RBRACE",
    "[": "LBRACKET",
    "]": "RBRACKET",
}

SYMBOL2NAME = {
    "@": "AT",
    "`": "BACKTICK",
    ":": "COLON",
    ",": "COMMA",
    ";": "SEMI",
    ".": "DOT",
    "|": "OR",
    "&": "AND",
    "+": "PLUS",
    "-": "MINUS",
    "=": "EQUAL",
    "*": "STAR"
}

ENDMARKER = r""


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

    def t_whitespace(self, s):
        r" \s+ "
        pass

    def t_nl(self, s):
        r"\n"
        pass

    def t_paren(self, s):
        r"[(){}[\]]"
        self.add_token(BRACKET2NAME[s], s)

    def t_arrow(self, s):
        r"->"
        self.add_token("ARROW", s)

    def t_subtype(self, s):
        r"<:"
        self.add_token("SUBTYPE", s)

    def t_typesep(self, s):
        r"::"
        self.add_token("TYPESEP", s)

    def t_symbol(self, s):
        r"[@:;*,.`|&+=-]"
        self.add_token(SYMBOL2NAME[s], s)

    def t_name(self, s):
        r"[A-Za-z_][A-Za-z_0-9]*"
        if s in RESERVED_WORDS:
            self.add_token(s.upper(), s)
        elif s in TRACE_WORDS:
            self.add_token("TRACE", s)
        else:
            self.add_token("IDENT", s)

    def t_literal(self, s):
        r"[0-9][0-9]*"
        self.add_token("LITERAL", s)


scan = TypeScanner()
parser = TypeParser(debug=BASIC_DEBUG)

parser.check_grammar()
parser.check_sets()
# parser.dump_grammar()


tokens = scan.tokenize(
    """
primitive red, green, blue
type color is (red | green | blue)

type map is Hashmap({string: int}, 78)


trait name
    name() -> String

type Greetable(T <: name) is String

trait calculator
    constant() -> int
    add(int, int) -> unsafe int


main(arg1: unsafe String, arg2: int) -> receive({color, string}) int
    let x : unsafe int = error
        y : receive(int|[int]) int = try receive catch expr
    try
        let a = receive
        expr
    catch
        expr
        expr
    end
    let x = expr


public hello(x: Greetable(Frog)) -> unsafe int
    let value = receive

"""
)



# interface bob
#     constant() -> int
#     accum() -> unsafe int
#     add(int, int) -> error :: int

# type map(K <: hashable, V) is OrderedMap(K, V)
# type Person(T) is {Int, T}
# type Person is {String, Int, (String | [String])}

# type Tuple is (String, Int)
# type Tuple is (String & Int)
# type Tuple is (String and Int)

# type Person is { String, Int, [String] }
# type Person is ( String & Int & [([String] | String)] )

# primitive red, green, blue


# for t in tokens:
#     print(t)
# print("------------------")


ast = parser.parse(tokens)
print("------------------")
print(ast)
