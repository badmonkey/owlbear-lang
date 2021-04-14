from enum import Enum, unique

from .statement import Expression


class ShiftExpr(Expression):
    """
    for infix ops: bsl bsr
    """

    @unique
    class Op(Enum):
        BIN_SHIFT_LEFT = 1
        BIN_SHIFT_RIGHT = 2


class OtherMultExpr(Expression):
    """
    for infix ops: / % **
    """

    @unique
    class Op(Enum):
        ARITH_DIV = 1
        ARITH_REM = 2
        ARITH_POW = 3


class PrefixExpr(Expression):
    """
    for prefix ops: - + not bnot
    """

    @unique
    class Op(Enum):
        ARITH_NEG = 1
        ARITH_POS = 2
        BOOL_NEGATE = 3
        BIN_NOT = 4
