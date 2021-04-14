from enum import Enum, unique

from .statement import Expression


class ListCompositeExpr(Expression):
    """
    for infix ops: ++ --
    """

    @unique
    class Op(Enum):
        LIST_ADD = 1
        LIST_SUB = 2
