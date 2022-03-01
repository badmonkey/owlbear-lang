import typing

from mixin import mixin

from .codegen import Codegen
from .expression import ExpressionMixin


class Let(Codegen):
    def __init__(self, name: str, value: ExpressionMixin):
        self._name: str = name
        self._expr: ExpressionMixin = value
