# test_optional_assign_none.py
# This should pass.

from typing import Optional
class B: pass
def f(x: Optional[B]):
    a: Optional[B] = None