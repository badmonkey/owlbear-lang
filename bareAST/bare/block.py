import typing

from .scope import Scope
from .typexpr import TypeExpr


class Statement:
    def emit(self, builder):
        pass


class Expression(Statement):
    def type(self) -> TypeExpr:
        pass

    def name(self) -> str:
        pass


class Block(Expression):
    def __init__(self, parent: typing.Optional[Scope]):
        self._scope: Scope = Scope(parent)
        self._body: typing.List[Statement] = []

    def scope(self) -> Scope:
        return None

    def append(self, stmt: Statement):
        pass


class LetStatement(Statement):
    def __init__(self, scope: Scope, name: str, value: Expression):
        pass


class DebugBlock(Block):
    pass


class CondClause:
    def __init__(self):
        self._predicate_expr = None


class CondBlock(Block):
    def __init__(self, parent: typing.Optional[Scope]):
        super().__init__(parent)
        self._clauses: typing.List[CondClause] = []


class TryBlock(Block):
    def __init__(self, parent: typing.Optional[Scope]):
        super().__init__(parent)
        self._catch: Block
        self._finally: typing.Optional[Block] = None
