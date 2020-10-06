class Expression:
    pass


class BinaryExpression(Expression):
    def __init__(self, op, expr):
        self.op = op
        self.terms = [expr]

    def add_term(self, expr):
        self.terms.append(expr)


class PrefixExpression(Expression):
    def __init__(self, op, expr):
        self.op = op
        self.expr = expr
