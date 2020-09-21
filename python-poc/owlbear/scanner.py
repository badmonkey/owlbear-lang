import re

from spark_parser.scanner import GenericScanner

from owlbear.token import OwlbearToken


RESERVED_WORDS = re.split(
    "\s+",
    """
type is primitive not or and error let
receive self try catch with finally case end
""",
)

UNSAFE_KEYWORDS = re.split("\s+", """let case""")

LONGSYMBOL_TOKENS = {
    "|>": "PIPE",
    "~>": "SEND",
    "->": "ARROW",
    "==": "EQUAL",
    "!=": "NOTEQ",
    "<=": "LTEQ",
    ">=": "GTEQ",
    "<-": "LEFTARROW",
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
    "<": "LT",
    ">": "GT",
    "*": "STAR",
    "/": "DIV",
    "!": "NOT",
    "(": "LPAREN",
    ")": "RPAREN",
    "{": "LBRACE",
    "}": "RBRACE",
    "[": "LBRACKET",
    "]": "RBRACKET",
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
            **kwargs,
        )
        self.rv.append(t)

    def error(self, s, mesg=None):
        symlen = len(s) if mesg else 1
        mesg = mesg or "Invalid character"
        print(f"Lexical error line {self.lineno + 1}: {mesg}")
        print('"' + self.lines[self.lineno] + '"')
        indent = " " * (self.pos - self.column0)
        underline = "^" * symlen
        print(f" {indent}{underline}")

        raise SystemExit

    def t_newline(self, s):
        r"\n"
        # print(f"COMPLETE.{self.lineno} [{self.lines[self.lineno]}]")
        self.lineno += 1
        self.column0 = self.pos + 1

    def t_whitespace(self, s):
        r"\s+"
        pass

    def t_blockstring(self, s):
        r"([\"]{3}(.|[\n])*?[\"]{3})|('{3}(.|[\n])*?'{3})"
        self.lineno += s.count("\n")
        self.add_token("STRING", s.strip(s[0]), text=s, is_literal=True)

    def t_string(self, s):
        r"('[^']*?')|(\"[^\"]*\")"
        self.lineno += s.count("\n")
        self.add_token("STRING", s.strip(s[0]), text=s, is_literal=True)

    def t_blockcomment(self, s):
        r"//>+([\s\S]*?)<+//"
        self.lineno += s.count("\n")
        block = s.strip("/")
        block = block.lstrip(">")
        block = block.rstrip("<")
        self.add_token("COMMENT", block, text=s)

    def t_comment(self, s):
        r"//[^\n]*(\n//[^\n]*)*"
        self.lineno += s.count("\n")
        block = "\n".join([ln.lstrip("/") for ln in s.splitlines()])
        self.add_token("COMMENT", block, text=s)

    def t_longsymbol(self, s):
        r"[|!~><=-][>=-]"
        if s in LONGSYMBOL_TOKENS:
            self.add_token(LONGSYMBOL_TOKENS[s], s)
            return

        self.safe_symbol(s[0])
        self.safe_symbol(s[1])

    def safe_symbol(self, s):
        if s not in SYMBOL_TOKENS:
            self.error(s)
        self.add_token(SYMBOL_TOKENS[s], s)

    def t_symbol(self, s):
        r"[(){}[\]<>@/:;*,.|&!+=-]"
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
            self.error(s, "Identifier can't be both literal and unsafe")

        if word in RESERVED_WORDS:
            if is_literal:
                self.error(s, "Keywords can't be used as literal names")
            if is_unsafe and word not in UNSAFE_KEYWORDS:
                self.error(s, "This keyword can't be marked as unsafe")

            self.add_token(word.upper(), word, **extra)
        else:
            self.add_token("IDENT", word, **extra)

    def t_number(self, s):
        r"(0x[0-9a-fA-F]+|0b[01]+|0o[0-7]+|\d+\.\d+|\d+)j?"
        self.add_token("NUMBER", s, text=s, is_literal=True)
