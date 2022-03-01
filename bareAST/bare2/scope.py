import typing

from mixin import Mixin


class Scope:
    def __init__(self, parent: Scope = None):
        self._parent: Scope = parent

    def add(self, name: str, typeT: str):
        pass

    def get_name(self) -> str:
        return "ha"


class ScopedMixin(Mixin):
    def init_scope(self, scope: Scope):
        self._scope = scope
        self._name = scope.get_name()

    def get_scope(self) -> Scope:
        return self._scope
