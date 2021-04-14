from enum import Enum, unique

from .statement import Expression


class ShortCircuitExpr(Expression):
    """
    for ops: and or
    """

    @unique
    class Op(Enum):
        BOOL_AND = 1
        BOOL_OR = 2
