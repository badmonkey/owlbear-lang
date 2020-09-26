import re

from spark_parser.scanner import GenericScanner

from owlbear.token import OwlbearToken

RESERVED_WORDS = re.split(
    r"\s+",
    """
type is primitive not or and error let
receive self try catch with finally case end
""",
)

UNSAFE_KEYWORDS = re.split(r"\s+", """let case cond receive""")

LONGSYMBOL_TOKENS = {
    "#[": "LAMBDA",
    "|>": "PIPE",
    "~>": "SEND",
    "->": "ARROW",
    "==": "CMPEQ",
    "!=": "CMPNOTEQ",
    "<=": "CMPLTEQ",
    ">=": "CMPGTEQ",
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
    "<": "CMPLT",
    ">": "CMPGT",
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

        # print(self.re.pattern)
        self.lineno = 0
        self.column0 = 1
        self.continue_after_error = False
        self.tokens = []
        self.lines = []

    def tokenize(self, s):
        self.tokens = []
        self.lines = s.splitlines()
        GenericScanner.tokenize(self, s)
        return self.tokens

    def add_token(self, name, s, **kwargs):
        t = OwlbearToken(
            kind=name,
            attr=s,
            line=self.lines[self.lineno],
            lineno=self.lineno,
            column=(self.pos - self.column0),
            **kwargs,
        )
        self.tokens.append(t)
        return t

    def error(self, s, mesg=None, symlen=None, quote=None):
        # pylint: disable=arguments-differ
        custom = mesg is not None
        symlen = symlen if custom else 1
        mesg = mesg or "Invalid character"

        t = self.add_token("BADTOKEN", s, text=mesg)

        print(f"Lexical error line {self.lineno + 1}: {mesg}")
        print(t.highlight(symlen=symlen, quote=quote))

        if self.continue_after_error and custom:
            return

        raise SystemExit

    @staticmethod
    def contains(s, tst):
        return any([c in s for c in tst])

    def t_newline(self, _s):
        r"\n"

        # before t_whitespace: so "\n" is matched before "\s"

        # print(f"COMPLETE.{self.lineno} [{self.lines[self.lineno]}]")
        self.lineno += 1
        self.column0 = self.pos + 1

    def t_whitespace(self, _s):
        r"\s+"

    def t_blockstring(self, s):
        r"([\"]{3}(.|[\n])*?[\"]{3})|('{3}(.|[\n])*?'{3})"

        # before string: so "'''" is matched before "'"
        self.lineno += s.count("\n")
        self.add_token("STRING", s.strip(s[0]), text=s, is_literal=True)

    def t_string(self, s):
        r"[a-zA-Z]?(('[^']*?')|(\"[^\"]*\"))"

        # before blockcomment,comment: so comments in strings are captured as strings
        cnt = s.count("\n")
        if cnt > 0:
            self.error(
                s, "Single quoted strings can't be multiline", symlen=s.find("\n"), quote="|"
            )
            self.lineno += cnt  # so lineno remains good if we skip the error
            return

        raw = s
        enc = "?"
        if s[0] not in "\"'":
            enc = s[0]
            s = s[1:]
        self.add_token("STRING", s.strip(s[0]), text=raw, encode=enc, is_literal=True)

    def t_blockcomment(self, s):
        r"//>+([\s\S]*?)<+//"

        # before comment: so "//>+" is matched before "//"
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

    def t_radix(self, s):
        r"(0x[_0-9A-Fa-f]+|0b[_01]+|0o[_0-7]+)"

        # before number: so "0x" is matched before "0"
        # scanner uses a looser regexp to catch common errors
        # ie '0x_ff' shouldn't be interpreted as the tokens '0' 'x_ff'

        if not re.fullmatch(
            r"(0x[0-9A-Fa-f](_?[0-9A-Fa-f])*|0b[01](_?[01])*|0o[0-7](_?[0-7])*)", s
        ):
            self.error(s, "Invalid formatted radix number literal")
            return
        self.add_token("NUMBER", s, text=s, is_literal=True)

    def t_number_like_names(self, s):
        r"_?\d+[.A-Za-z_0-9+-]*[?!]?"

        # before number,name: so "_?D+A+" are matched as names not numbers
        # before number,name: so "D_.D" is matched as a number not multiple tokens
        if OwlbearScanner.contains(s, ".+-"):
            self.t_number(s)
        elif re.match(r"^_\d+", s) or re.match(r".*[A-DF-Za-df-z?!]", s):
            self.t_name(s)
        else:
            self.t_number(s)

    def t_number(self, s):
        r"(([_\d]*\.[_\d]+) | (_?\d[_\d]*\.?))([eE][+-]?[_\d]+)?"

        # before symbol: so ".XXX" is matched in floats before matched as single "."
        # As with radix, we match with a looser regexp to catch typos

        if not re.fullmatch(r"(((\d(_?\d)*)?\.\d(_?\d)*)|(\d(_?\d)*\.?))([eE][+-]?\d(_?\d)*)?", s):
            self.error(s, "Invalid formatted number literal")
            return

        self.add_token("NUMBER", s, text=s, is_literal=True)

    def t_longsymbol(self, s):
        r"[|!~><=-][>=-]"

        # before symbol: so "xY" is matched before "x"
        if s in LONGSYMBOL_TOKENS:
            self.add_token(LONGSYMBOL_TOKENS[s], s)
            return

        self.safe_symbol(s[0])
        self.safe_symbol(s[1])

    def t_name(self, s):  # noqa
        r"\#?[A-Za-z_0-9]+[?!]?"

        # before symbol: so "#XX" matched before "#"
        # match keywords and identifiers
        # handle special forms "#xxxx", "xxxx?", and "xxxx!"

        if not re.fullmatch(r"\#?[A-Za-z_][A-Za-z_0-9]*[?!]?", s):
            self.error(s, "Invalid formatted identifier")
            return

        is_literal = s[0] == "#"
        is_unsafe = s[-1] == "?"
        is_type = s[-1] == "!"

        word = s
        extra = {}
        if is_literal:
            extra["is_literal"] = True
            word = word[1:]
        if is_unsafe:
            extra["is_unsafe"] = True
            word = word[0:-1]
        if is_type:
            extra["is_type"] = True
            word = word[0:-1]

        if is_literal or is_unsafe or is_type:
            extra["text"] = s

        if is_literal and is_unsafe:
            self.error(s, "Identifier can't be both literal and unsafe")
            return
        if is_literal and is_type:
            self.error(s, "Identifier can't be both literal and type constructor")
            return

        if word in RESERVED_WORDS:
            if is_literal:
                self.error(s, "Keywords can't be used as literal names")
                return
            if is_type:
                self.error(s, "Keywords can't be used as type constructors")
                return
            if is_unsafe and word not in UNSAFE_KEYWORDS:
                self.error(s, "This keyword can't be marked as unsafe")
                return

            self.add_token(word.upper(), word, **extra)
        else:
            self.add_token("IDENT", word, **extra)

    def safe_symbol(self, s):
        if s not in SYMBOL_TOKENS:
            self.error(s)
            return
        self.add_token(SYMBOL_TOKENS[s], s)

    def t_symbol(self, s):
        r"[(){}[\]<>@/:;*,.|&!+=-]"
        self.add_token(SYMBOL_TOKENS[s], s)
