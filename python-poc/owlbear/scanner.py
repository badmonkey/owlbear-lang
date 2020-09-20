import re

from spark_parser.scanner import GenericScanner

from owlbear.token import OwlbearToken


RESERVED_WORDS = re.split(
    "\s+",
    """
type is primitive or and error let
receive self try catch with finally case end
""",
)

UNSAFE_KEYWORDS = re.split("\s+", """let case""")


BRACKET_TOKENS = {
    "(": "LPAREN",
    ")": "RPAREN",
    "{": "LBRACE",
    "}": "RBRACE",
    "[": "LBRACKET",
    "]": "RBRACKET",
}

SYMBOL_TOKENS = {
    "@": "AT",
    ":": "TYPESEP",
    ",": "COMMA",
    ";": "SEMI",
    ".": "DOT",
    "|": "OR",
    "&": "AND",
    "+": "PLUS",
    "-": "MINUS",
    "=": "EQUAL",
    "*": "STAR",
}

LONGSYMBOL_TOKENS = {
    "|>": "PIPE",
    "->": "ARROW",
    "<-": "LEFTARROW",
    "~>": "SEND",
}

END_TOKEN = r""


# delattr(GenericScanner, "t_default")


class OwlbearScanner(GenericScanner):
    """
    Token scanner for owlbear sugary frontend
    """

    def __init__(self):
        GenericScanner.__init__(self)
        self.lineno = 0
        self.column0 = 1

    def tokenize(self, input):
        self.rv = []
        self.lines = input.splitlines()
        GenericScanner.tokenize(self, input)
        return self.rv

    def add_token(self, name, s, **kwargs):
        t = OwlbearToken(
            kind=name,
            attr=s,
            line=self.lines[self.lineno],
            column=(self.pos - self.column0),
            **kwargs
        )
        self.rv.append(t)

    def t_newline(self, s):
        r"\n"
        self.lineno += 1
        self.column0 = self.pos + 1

    def t_whitespace(self, s):
        r"\s+"
        pass

    def t_paren(self, s):
        r"[(){}[\]]"
        self.add_token(BRACKET_TOKENS[s], s)

    def t_longsymbol(self, s):
        r"(\|>)|(<-)|(->)|(~>)"
        self.add_token(LONGSYMBOL_TOKENS[s], s)

    def t_symbol(self, s):
        r"[@:;*,.|&+=-]"
        self.add_token(SYMBOL_TOKENS[s], s)

    def t_name(self, s):
        r"\#?[A-Za-z_][A-Za-z_0-9]*\??"
        is_literal = s[0] == "#"
        is_unsafe = s[-1] == "?"

        word = s
        extra = {}
        if is_literal:
            extra["is_literal"] = True
            word = word[1:]
        if is_unsafe:
            extra["is_unsafe"] = True
            word = word[0:-1]

        if is_literal or is_unsafe:
            extra["text"] = s

        if is_literal and is_unsafe:
            self.add_token("BADTOKEN", word, **extra)
            return

        kind = "IDENT"

        if word in RESERVED_WORDS:
            if is_literal or (is_unsafe and word not in UNSAFE_KEYWORDS):
                kind = "BADTOKEN"
            else:
                kind = word.upper()

        self.add_token(kind, word, **extra)

    def t_string(self, s):
        r"(?:[\"]{3}(.|[\n])*[\"]{3})|('{3}(.|[\n])*'{3})|('[^']*')|(\"[^\"]*\")"
        if s[0] == '"':
            data = s.strip('"')
        else:
            data = s.strip("'")

        self.add_token("STRING", data, text=s, is_literal=True)

    def t_number(self, s):
        r"(0x[0-9a-f]+|0b[01]+|0o[0-7]+|\d+\.\d|\d+)j?"
        self.add_token("NUMBER", s, text=s, is_literal=True)
