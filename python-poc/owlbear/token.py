from spark_parser.scanner import GenericToken


class OwlbearToken(GenericToken):
    def __init__(self, kind, attr, line, column, **kwargs):
        super().__init__(kind, attr)
        self.line = line
        self.column = column
        self.extra = kwargs

    def __str__(self):
        return super().__str__() + ", extra: " + str(self.extra)

    def text(self):
        if "text" in self.extra:
            return self.extra["text"]
        return self.attr
