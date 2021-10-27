# test_optional_assign_subclass.py
# This should pass.

from typing import Optional
class B: pass
class D(B): pass
def f(x: D):
    a: Optional[B] = x