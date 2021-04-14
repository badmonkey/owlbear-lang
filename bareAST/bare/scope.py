import typing

from .error import fatalError, warnOrError


class Named:
    def __init__(
        self, name: str, context: "Named" = None, private: bool = False, inner: bool = False
    ):
        self._name: str = name
        sep = "$" if inner else "."
        self._longname: str = (
            f"{context.longname}{sep}{name}" if context is not None else self._name
        )
        self._private: bool = private or inner

    @property
    def name(self) -> str:
        return self._name

    @property
    def longname(self) -> str:
        return self._longname

    @property
    def llvm_name(self) -> str:
        prefix = "_priv$" if self._private else ""
        return f"{prefix}{self._longname}"

    @property
    def llvm_value(self) -> str:
        pass

    def extend(self, name: str) -> str:
        return f"{self._longname}.{name}"

    @property
    def qualified(self) -> bool:
        return self._longname.count(".") > 0

    def defined_at(self):
        pass

    def type(self):
        pass


class Value(Named):
    pass


class Constant(Named):
    pass


class Scope:
    def __init__(self, parent: typing.Optional["Scope"] = None, share_temps: bool = True):
        self._parent: typing.Optional["Scope"] = parent
        self._values: typing.Dict[str, Named] = {}
        self._share_temps: bool = share_temps
        self._tmpname: int = 0

    @property
    def next_tmp(self) -> int:
        tmp = self._tmpname
        self._tmpname += 1
        return tmp

    def llvm_temp(self) -> str:
        if self._share_temps and self._parent:
            return self._parent.llvm_temp()
        return f"%{self.next_tmp}"

    def exists(self, name: str) -> bool:
        if name in self._values:
            return True
        if self._parent is None:
            return False
        return self._parent.exists(name)

    def get(self, name: str, usage) -> Named:
        if name in self._values:
            return self._values[name]
        if self._parent is None:
            fatalError("Unknown symbol", name=name, used=usage)
        return self._parent.get(name, usage)

    def add(self, item: Named):
        if item.name in self._values:
            fatalError(
                "redefined value",
                name=item.name,
                new_def=item.defined_at(),
                defined=self._values[item.name].defined_at(),
            )

        if self._parent and self._parent.exists(item.name):
            warnOrError(
                "shadowing existing name",
                name=item.name,
                new_def=item.defined_at(),
                defined=self.get(item.name, None).defined_at(),
            )

        self._values[item.name] = item
