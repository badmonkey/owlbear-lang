import re

from spark_parser.scanner import GenericScanner

from .token import OwlbearToken, Kind, RESERVED_WORDS
import owl.filter as filter

TABSTOP = 8

LONGSYMBOLS = {
    "#[": Kind.LAMBDA,
    "~>": Kind.SEND,
    "->": Kind.ARROW,
    "==": Kind.CMPEQ,
    "!=": Kind.CMPNOTEQ,
    "<>": Kind.CMPNOTEQ,
    "<=": Kind.CMPLTEQ,
    ">=": Kind.CMPGTEQ,
    "<-": Kind.LEFTARROW,
    "<<": Kind.LBITSTRING,
    ">>": Kind.RBITSTRING,
    "++": Kind.LSTADD,
    "--": Kind.LSTSUB,
    "**": Kind.POW,
}

SYMBOLS = {
    "@": Kind.AT,
    ":": Kind.TYPESEP,
    ",": Kind.COMMA,
    ";": Kind.SEMI,
    ".": Kind.DOT,
    "|": Kind.OR,
    "&": Kind.AND,
    "^": Kind.BXOR,
    "+": Kind.PLUS,
    "-": Kind.MINUS,
    "=": Kind.EQUAL,
    "<": Kind.CMPLT,
    ">": Kind.CMPGT,
    "*": Kind.STAR,
    "/": Kind.DIV,
    "%": Kind.REM,
    "~": Kind.BNOT,
    "!": Kind.EXCLAIM,
    "?": Kind.QMARK,
    "#": Kind.HASH,
    "(": Kind.LPAREN,
    ")": Kind.RPAREN,
    "{": Kind.LBRACE,
    "}": Kind.RBRACE,
    "[": Kind.LBRACKET,
    "]": Kind.RBRACKET,
}


INDENT_DELIMITERS = {Kind.BEGIN, Kind.UNDENT, Kind.NEWLINE}



# delattr(GenericScanner, "t_default")


class OwlbearScanner(GenericScanner):
    """
    Token scanner for owlbear sugary frontend
    """

    def __init__(self):
        GenericScanner.__init__(self)

        # print(self.re.pattern)
        self.lineno = 0
        self.column0 = 0
        self.is_newline = True
        self.indents = [0]
        self.continue_after_error = False
        self.tokens = []
        self.lines = []

    def tokenize(self, s):
        self.tokens = []

        s = s.rstrip("\n")
        self.lines = s.splitlines()

        GenericScanner.tokenize(self, s)

        self.lines.append("")
        self.add_token(Kind.NEWLINE, ' ', is_newline=True)
        self.add_token(Kind.ENDMARK, r"")

        self.tokens = list(filter.pipeline(self.tokens))

        return self.tokens

    def add_token(self, name, s, is_newline=False, offset=0, **kwargs):
        t = OwlbearToken(kind=name,
                         attr=s,
                         line=self.lines[self.lineno],
                         lineno=self.lineno,
                         column=(self.pos - self.column0 + offset),
                         **kwargs)
        if is_newline:
            self.lineno += 1
            self.column0 = self.pos + 1

        if self.is_newline and name not in INDENT_DELIMITERS:
            while 0 < self.indents[-1]:
                self.indents = self.indents[0:-1]
                self.tokens.append(
                    OwlbearToken(kind=Kind.UNDENT,
                                 attr='}',
                                 line=self.lines[self.lineno],
                                 lineno=self.lineno,
                                 column=(self.pos - self.column0 + offset)))

        self.is_newline = is_newline

        self.tokens.append(t)

    def handle_indent(self, s):
        indent = len(s)
        if indent > self.indents[-1]:
            self.add_token(Kind.BEGIN, '{')
            self.indents.append(indent)
        if indent == self.indents[-1]:
            self.is_newline = False
        else:
            # May need several levels of dedent
            while indent < self.indents[-1]:
                self.indents = self.indents[0:-1]
                self.add_token(Kind.UNDENT, '}')

        return

    def error(self, s, mesg=None, symlen=None, quote=None):
        # pylint: disable=arguments-differ
        custom = mesg is not None
        symlen = symlen if custom else 1
        mesg = mesg or "Invalid character"

        t = self.add_token(Kind.BAD, s, text=mesg)

        print(f"Lexical error line {self.lineno + 1}: {mesg}")
        print(t.highlight(symlen=symlen, quote=quote))

        if self.continue_after_error and custom:
            return

        raise SystemExit

    def as_string(self):
        s = ""
        istr = ""
        prevl = 0

        for t in self.tokens:
            match t.kind:
                case Kind.BEGIN:
                    s += f"{{"
                    istr += "    "
                case Kind.END:
                    istr = istr[:-4]
                    s += f"\n{istr}}}"
                case _:
                    if t.lineno != prevl:
                        s += f"\n{istr}"
                        prevl = t.lineno
                    s += f"{t.text()} "
        return s

    @staticmethod
    def contains(s, tst):
        return any([c in s for c in tst])

    def t_newline(self, _s):
        r"\n"

        # before t_whitespace: so "\n" is matched before "\s"
        self.add_token(Kind.NEWLINE, ' ', is_newline=True)

    def t_whitespace(self, _s):
        r"\s+"

        #  if (c == ' ') {
        #       col++
        # if (c == '\t') {
        #       col = (col / tok->tabsize + 1) * tok->tabsize;
        if self.is_newline:
            self.handle_indent(_s)

    def t_blockstring(self, s):
        r"([\"]{3}(.|[\n])*?[\"]{3})|('{3}(.|[\n])*?'{3})"

        # before string: so "'''" is matched before "'"
        self.lineno += s.count("\n")
        self.add_token(Kind.STRING, s.strip(s[0]), text=s, is_literal=True)

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
        enc = None
        if s[0] not in "\"'":
            enc = s[0]
            s = s[1:]
        self.add_token(Kind.STRING,
                       s.strip(s[0]),
                       text=raw,
                       encode=enc if enc else "utf8",
                       is_literal=True,
                       offset=2 if enc else 1)

    def t_blockcomment(self, s):
        r"//>+([\s\S]*?)<+//"

        # before comment: so "//>+" is matched before "//"
        self.lineno += s.count("\n")
        block = s.strip("/")
        block = block.lstrip(">")
        block = block.rstrip("<")
        self.add_token(Kind.COMMENT, block, text=s)

    def t_comment(self, s):
        r"//[^\n]*"

        v = s.lstrip("/")
        self.add_token(Kind.COMMENT, v, text=s, offset=2)

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
        self.add_token(Kind.NUMBER, s, text=s, is_literal=True)

    def t_number_like_names(self, s):
        r"_?\d+[.A-Za-z_0-9+-]*"

        # before number,name: so "_?D+A+" are matched as names not numbers
        # before number,name: so "D_.D" is matched as a number not multiple tokens
        if OwlbearScanner.contains(s, ".+-"):
            self.t_number(s)
        elif re.match(r"^_\d+", s) or re.match(r".*[A-DF-Za-df-z]", s):
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

        self.add_token(Kind.NUMBER, s, text=s, is_literal=True)

    def t_longsymbol(self, s):
        r"[!*~+><=-][*+<>=-]"

        # before symbol: so "xY" is matched before "x"
        if s in LONGSYMBOLS:
            self.add_token(LONGSYMBOLS[s], s)
            return

        # not a long symbol so tokenise the chars seperately
        self.safe_symbol(s[0])
        self.safe_symbol(s[1])

    def t_name(self, s):
        r"[A-Za-z_0-9]+"

        # match keywords and identifiers
        # handle special forms "xxxx?"

        if not re.fullmatch(r"[A-Za-z_][A-Za-z_0-9]*", s):
            self.error(s, "Invalid formatted identifier")
            return

        if s in RESERVED_WORDS:
            self.add_token(s.upper(), s)
        else:
            self.add_token(Kind.NAME, s)

    def t_symbol(self, s):
        r"[(){}[\]<>@/:;*,.%^|&#?!+=-]"
        self.add_token(SYMBOLS[s], s)

    def safe_symbol(self, s):
        if s not in SYMBOLS:
            self.error(s)
            return
        self.add_token(SYMBOLS[s], s)
