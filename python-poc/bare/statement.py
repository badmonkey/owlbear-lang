import typing

from .scope import Scope
from .typexpr import TypeExpr


class Statement:
    def __init__(self):
        pass

    def type(self) -> TypeExpr:
        pass

    def llvm_name(self) -> str:
        pass


class Expression(Statement):
    pass


class Sequence(Statement):
    def __init__(self):
        super().__init__()
        self._body: typing.List[Statement] = []


class LetStatement(Statement):
    def __init__(self):
        super().__init__()
        self._value: Expression


class DebugStatement(Statement):
    def __init__(self):
        super().__init__()
        self._scope: Scope
        self._body: Statement


class ReceiveStatement(Statement):
    pass


class CondClause:
    def __init__(self):
        self._predicate_expr = None
        self._scope: Scope = None
        self._body: Statement = None


class CondStatement(Statement):
    def __init__(self):
        super().__init__()
        self._clauses: typing.List[CondClause] = []


class TryStatement(Statement):
    def __init__(self):
        super().__init__()
        self._scope: Scope
        self._body: Statement
        self._catch: Statement
        self._finally: typing.Optional[Statement] = None
