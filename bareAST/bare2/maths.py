import typing
from enum import Enum, unique

from mixin import mixin

from .codegen import Codegen
from .expression import ExpressionMixin
from .varargs import VarargsMixin


@mixin(VarargsMixin, ExpressionMixin)
class Commutative(Codegen):
    @unique
    class Op(Enum):
        ARITH_SUM = 1
        ARITH_MULT = 2
        BIN_AND = 3
        BIN_OR = 4
        BIN_XOR = 5

    def __init__(self, op: Op, left: ExpressionMixin, right: ExpressionMixin):
        self._op: Commutative.Op = op
        self._exprs: typing.List[ExpressionMixin] = []

        self._exprs.append(left)
        self._exprs.append(right)


@mixin(ExpressionMixin)
class Shift(Codegen):
    @unique
    class Op(Enum):
        BIN_SHIFT_LEFT = 1
        BIN_SHIFT_RIGHT = 2

    def __init__(self, op: Op, expr: ExpressionMixin, shift: ExpressionMixin):
        self._op: Shift.Op = op
        self._expr: ExpressionMixin = expr
        self._shift: ExpressionMixin = shift


@mixin(ExpressionMixin)
class Division(Codegen):
    @unique
    class Op(Enum):
        ARITH_DIV = 1
        ARITH_REM = 2
        ARITH_POW = 3

    def __init__(self, op: Op, expr: ExpressionMixin, factor: ExpressionMixin):
        self._op: Division.Op = op
        self._expr: ExpressionMixin = expr
        self._factor: ExpressionMixin = factor
