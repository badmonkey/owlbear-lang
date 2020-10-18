import typing

from .module import Module, Package


class Target:
    def __init__(self):
        self._module_paths: typing.List[str] = []
        self._source_root: str
        self._build_root: str
        self._packages: typing.Dict[str, Package] = {}

    def package(self, longname: str) -> "Package":
        if longname in self._packages:
            return self._packages[longname]
        tmp = Package(longname)
        self._packages[longname] = tmp
        return tmp

    def module(self, longname: str) -> "Module":
        packname = longname.split(".")
        modname = packname.pop()
        pack = self.package(".".join(packname))
        return pack.module(modname)

    def print(self):
        print("llvm")
        print("-" * 20)
        for _, p in self._packages.items():
            p.print()


"""
loadpaths ::= path ; loadpaths
path ::= <fs-dir-path>
path ::= <zip-file>

$path$/<package-path>/module.ll
$path$/<package-path>/moduleN.ll
$path$/<package-path>/_package.bare

path/org/owlbear/std/module.ll
path/org/owlbear/std/_package.bare



src/<package-path>/module.owl

build/package.bare
build/<package-path>/module.ll
build/<package-path>/module.bare
"""
