import typing
from enum import Enum, unique

from .block import Expression


class ShortCircuitExpr(Expression):
    """
    for ops: `and or` where second value may or not need to be evaluated.
    """

    @unique
    class Op(Enum):
        ALL_TRUE = 1
        ANY_TRUE = 2

    def __init__(self, op: Op, left: Expression, right: Expression):
        self._op: ShortCircuitExpr.Op = op
        self._exprs: typing.List[Expression] = []

        self._exprs.append(left)
        self._exprs.append(right)

    def append(self, expr: Expression):
        self._exprs.append(expr)
