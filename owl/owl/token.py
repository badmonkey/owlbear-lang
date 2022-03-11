import enum
import re
from spark_parser.scanner import GenericToken


class StrEnum(str, enum.Enum):
    def __new__(cls, value, *args, **kwargs):
        if not isinstance(value, (str, enum.auto)):
            raise TypeError(
                f"Values of StrEnums must be strings: {value!r} is a {type(value)}"
            )
        return super().__new__(cls, value, *args, **kwargs)

    def __str__(self):
        return str(self.value)

    def _generate_next_value_(name, *_):
        return name


_TOKEN_KINDS = """
  BEGIN UNDENT NEWLINE BAD STRING COMMENT NUMBER NAME ENDMARK
  LAMBDA SEND ARROW LEFTARROW LBITSTRING RBITSTRING
  CMPEQ CMPNOTEQ CMPLTEQ CMPGTEQ CMPLT CMPGT
  LSTADD LSTSUB STAR EXCLAIM QMARK HASH
  AT TYPESEP COMMA SEMI DOT PLUS MINUS EQUAL
  LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
"""

_RESERVED_WORDS = """
  is as or
  not and let try bor bsl bsr div rem pow end
  type with self case cond bnot band true bxor
  error catch false
  import memory
  primitive receive finally undefined otherwise
"""


RESERVED_WORDS = {w for w in re.split(r"\s+", _RESERVED_WORDS) if w}


Kind = StrEnum('Kind', _TOKEN_KINDS + _RESERVED_WORDS.upper())



class OwlbearToken(GenericToken):
    TEXT_COLR = "\033[93m"
    ERROR_COLR = "\033[91m"
    END_COLR = "\033[0m"

    def __init__(self, kind, attr, line, lineno, column, **kwargs):
        # pylint: disable=too-many-arguments
        super().__init__(kind, attr)
        self.line = line
        self.lineno = lineno
        self.column = column
        self.extra = kwargs

    def __str__(self):
        return super().__str__() + f", l.{self.lineno + 1} c.{self.column}" + ((", extra: " + str(self.extra)) if self.extra else "")

    def text(self):
        if "text" in self.extra:
            return self.extra["text"]
        return self.attr

    def highlight(self, symlen=None, quote=None):
        prefix = f"line:{self.lineno + 1} "
        symlen = symlen or len(self.attr)
        quote = quote or '"'
        indent = " " * (len(prefix) + len(quote) + self.column)
        underline = "^" * symlen

        # return f"{prefix}{self.TEXT_COLR}{quote}{self.line}{quote}{self.END_COLR}\n{indent}{self.ERROR_COLR}{underline}{self.END_COLR}"
        return f"{prefix}{quote}{self.line}{quote}\n{indent}{underline}"
