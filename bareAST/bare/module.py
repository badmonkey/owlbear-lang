import typing

from llvmlite import ir

from .scope import Named, Scope


class Package(Named):
    def __init__(self, name: str):
        super().__init__(name)

        self._modules: typing.Dict[str, Module] = {}

    def module(self, name: str) -> "Module":
        if name in self._modules:
            return self._modules[name]
        tmp = Module(name, package=self)
        self._modules[name] = tmp
        return tmp

    def print(self):
        for _, m in self._modules.items():
            m.print()
            print("-" * 20)


class Module(Named):
    def __init__(self, name: str, package: Package):
        super().__init__(name, context=package)

        self._package: Package = package
        self._symbols: Scope = Scope()

        if not self.qualified:
            raise Exception(f"Invalid module name [{self.longname}]")

        self.llvm = ir.Module(name=self.llvm_name)

    def add(self, item: Named):
        self._symbols.add(item)
        # item.codegen()

    def print(self):
        print(self.llvm)
