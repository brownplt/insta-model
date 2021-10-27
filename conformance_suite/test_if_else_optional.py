# test_if_else_optional.py
# This should pass.

from typing import Optional
class C:
    def __init__(self):
        self.field = self
def g(x: C):
    pass
def f(x: Optional[C], y: Optional[C]):
    if x is None:
        x = y
        if x is None:
            return None
        else:
            return g(x)
    else:
        return g(x)
    return None