# method_generative.py
# This should pass.
# This should terminate.

from typing import Any, ClassVar


class C:
    m1: ClassVar[Any] = lambda self: 2
    def m2(self):
        return 3


obj = C()
assert obj.m1 is not obj.m1
assert obj.m2 is not obj.m2