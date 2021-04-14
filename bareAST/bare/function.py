import typing

from llvmlite import ir

from .error import fatalError
from .module import Module
from .scope import Named


class Function(Named):
    def __init__(self, name: str, context: Named, private: bool = False):
        super().__init__(
            name, context=context, private=private, inner=isinstance(context, Function)
        )

        self._module: Module

        if isinstance(context, Module):
            self._module = context
        elif isinstance(context, Function):
            self._module = context.module
        else:
            raise fatalError("No context for function", name=name)

    @property
    def module(self) -> Module:
        return self._module

    def codegen(self):
        double = ir.DoubleType()
        fnty = ir.FunctionType(double, (double, double))

        func = ir.Function(self.module.llvm, fnty, name=self.llvm_name)

        # Now implement the function
        block = func.append_basic_block(name="entry")
        builder = ir.IRBuilder(block)
        a, b = func.args
        result = builder.fadd(a, b, name="res")
        builder.ret(result)
