from enum import Enum, unique

from .block import Expression


class ShiftExpr(Expression):
    """
    for infix ops: `bsl bsr` TYPE op NUM
    """

    @unique
    class Op(Enum):
        BIN_SHIFT_LEFT = 1
        BIN_SHIFT_RIGHT = 2

    def __init__(self, op: Op, expr: Expression, shift: Expression):
        self._op: ShiftExpr.Op = op
        self._expr: Expression = expr
        self._shift: Expression = shift


class OtherArithExpr(Expression):
    """
    for infix ops: `/ % **`
    """

    @unique
    class Op(Enum):
        ARITH_DIV = 1
        ARITH_REM = 2
        ARITH_POW = 3

    def __init__(self, op: Op, expr: Expression, factor: Expression):
        self._op: OtherArithExpr.Op = op
        self._expr: Expression = expr
        self._factor: Expression = factor


class PrefixExpr(Expression):
    """
    for prefix ops: `- + not bnot`
    """

    @unique
    class Op(Enum):
        ARITH_NEG = 1
        ARITH_POS = 2
        BOOL_NEGATE = 3
        BIN_NOT = 4

    def __init__(self, op: Op, expr: Expression):
        self._op: PrefixExpr.Op = op
        self._expr: Expression = expr
