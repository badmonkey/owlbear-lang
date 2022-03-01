import typing

from mixin import Mixin, mixin

from .codegen import Codegen
from .expression import ExpressionMixin
from .let import Let
from .scope import Scope


class Block(Codegen):
    def __init__(self, parent: Scope):
        self._statements: typing.List[Codegen] = []

    def eval(self, expr: ExpressionMixin):
        self._statements.append(expr)

    def letvar(self, name: str, value: ExpressionMixin):
        self._statements.append(Let(name, value))
