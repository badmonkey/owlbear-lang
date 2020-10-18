import typing

from .statement import Expression
from .typexpr import TypeExpr


class OrExpr(Expression):
    pass


class AndExpr(Expression):
    pass


class CompareExpr(Expression):
    pass


class SumExpr(Expression):
    pass


class ShiftExpr(Expression):
    pass


class MultExpr(Expression):
    pass


class DivRemExpr(Expression):
    pass


class PrefixExpr(Expression):
    pass


class LiteralExpr(Expression):
    pass


class ErrorExpr(Expression):
    pass


class VarExpr(Expression):
    pass
