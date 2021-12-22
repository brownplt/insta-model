# methods_can_be_declared_as_class_variables.py
# This should pass.
# This should terminate.

from typing import ClassVar, Any

class C:
    x: ClassVar[Any] = lambda self, n: n + 1

o = C()
assert o.x(2) is 3
