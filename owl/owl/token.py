from spark_parser.scanner import GenericToken


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
        return super().__str__() + ((", extra: " + str(self.extra)) if self.extra else "")

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
