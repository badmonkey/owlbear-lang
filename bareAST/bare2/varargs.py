import typing

from mixin import Mixin

from .expression import ExpressionMixin


class VarargsMixin(Mixin):
    def append(self, expr: ExpressionMixin):
        self._exprs.append(expr)
