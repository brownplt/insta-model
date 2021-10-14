# subclass_builtin.py
# This should pass.

from __static__ import cast

class C(int):
    pass

x: C = C(42)
y: int = x
