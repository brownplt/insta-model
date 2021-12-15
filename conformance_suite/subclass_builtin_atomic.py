# subclass_builtin_atomic.py
# This should pass.

class C(int):
    pass

x: C = C(42)
y: int = x
