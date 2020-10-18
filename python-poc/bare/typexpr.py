import typing

from .scope import Named


class TypeValue(Named):
    pass


class TypeTemplate(Named):
    pass


class TypeExpr:
    def covers(self, t: "TypeExpr") -> bool:
        """ t is a subset of self """

    def total(self, t: "TypeExpr") -> bool:
        """ self and t cover the same type """

    def difference(self, other: "TypeExpr") -> "TypeExpr":
        """ types that are in self but not other """

    def align(self) -> int:
        pass

    def bytes(self) -> int:
        pass

    def text(self, expand: bool = False) -> str:
        pass


class TypeVar(TypeExpr):
    pass


class TypeAny(TypeExpr):
    pass


class TypeNone(TypeExpr):
    pass


class TypeAlias(TypeExpr):
    def __init__(self):
        self._name: str
        self._type: TypeExpr

    def text(self, expand=False):
        return self._type.text(expand) if expand else self._name


class TypeArray(TypeExpr):
    def __init__(self):
        self._elementT: TypeExpr
        self._size: int

    def align(self):
        return self._elementT.align()

    def bytes(self):
        return self._elementT * self._size

    def text(self, expand=False):
        return f"[{self._elementT.text(expand)} * {self._size}]"


class TypeTuple(TypeExpr):
    def __init__(self):
        self._elements: typing.List[TypeExpr] = []


class TypeList(TypeExpr):
    def __init__(self):
        self._elementT: TypeExpr

    def text(self, expand=False):
        return f"[{self._elementT.text(expand)}]"


class Field:
    def __init__(self):
        self._name: str
        self._type: TypeExpr


class TypeRecord(TypeExpr):
    def __init__(self):
        self._fields: typing.List[Field] = []
        self._info: typing.Dict[str, Field] = {}


class TypeInt(TypeExpr):
    pass


class TypeFloat(TypeExpr):
    pass


class TypeString(TypeExpr):
    pass


class TypeBinary(TypeExpr):
    pass


class TypePartialError(TypeExpr):
    pass


class TypeActor(TypeExpr):
    pass


class TypeUnion(TypeExpr):
    def __init__(self):
        self._elements: typing.List[TypeExpr] = []


class TypeFunction(TypeExpr):
    def __init__(self):
        self._params: typing.List[TypeExpr] = []
        self._return: TypeExpr


class Type:
    @staticmethod
    def instance(template: TypeTemplate, **kargs) -> TypeExpr:
        pass

    @staticmethod
    def union(*args) -> TypeExpr:
        pass

    @staticmethod
    def list(elem: TypeExpr) -> TypeExpr:
        pass

    @staticmethod
    def record(**kwargs) -> TypeExpr:
        pass
