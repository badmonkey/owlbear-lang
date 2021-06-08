from enum import Enum, unique

from .block import Expression


class CompareExpr(Expression):
    """
    for infix ops: `== != < > <= >=`
    """

    @unique
    class Op(Enum):
        CMP_EQ = 1
        CMP_NE = 2
        CMP_LT = 3
        CMP_GT = 4
        CMP_LTEQ = 5
        CMP_GTEQ = 6
