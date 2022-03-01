import typing

from mixin import Mixin, mixin


class ExpressionMixin(Mixin):
    def typexpr(self):
        pass

    def value(self) -> str:
        return self._name
