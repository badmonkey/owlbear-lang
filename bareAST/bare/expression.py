from .statement import Expression


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
