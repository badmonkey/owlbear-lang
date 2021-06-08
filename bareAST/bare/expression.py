from .block import Expression


class LiteralExpr(Expression):
    """
    for: string int float binary tuple list map
    """


class ErrorExpr(Expression):
    pass


class VarExpr(Expression):
    pass


class ReceiveExpr(Expression):
    pass


class MemoryExpr(Expression):
    pass


class IfElseExpr(Expression):
    def __init__(self, cond: Expression, iftrue: Expression, iffalse: Expression):
        self._cond: Expression = cond
        self._iftrue: Expression = iftrue
        self._iffalse: Expression = iffalse
