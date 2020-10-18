import llvmlite.binding as llvm
from bare.function import Function
from bare.module import Module, Package
from bare.scope import Constant, Named, Scope, Value
from bare.statement import (
    CondStatement,
    DebugStatement,
    ReceiveStatement,
    Sequence,
    Statement,
    TryStatement,
)
from bare.target import Target

llvm.initialize()
llvm.initialize_all_targets()
llvm.initialize_native_asmprinter()


target = llvm.Target.from_default_triple()
_machine = target.create_target_machine()
