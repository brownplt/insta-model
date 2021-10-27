# test_optional_assign_subclass_opt.py
# This should pass.

from typing import Optional
class B: pass
class D(B): pass
def f(x: Optional[D]):
    a: Optional[B] = x