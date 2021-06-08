import typing
from enum import Enum, unique

from .block import Expression


class CommutativeExpr(Expression):
    """
    for ops: `+ * band bor bxor` where the order of eval is not important
    """

    @unique
    class Op(Enum):
        ARITH_SUM = 1
        ARITH_MULT = 2
        BIN_AND = 3
        BIN_OR = 4
        BIN_XOR = 5

    def __init__(self, op: Op, left: Expression, right: Expression):
        self._op: CommutativeExpr.Op = op
        self._exprs: typing.List[Expression] = []

        self._exprs.append(left)
        self._exprs.append(right)

    def append(self, expr: Expression):
        self._exprs.append(expr)

    def emit(self, builder):
        pass
