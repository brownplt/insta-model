# test_if_optional_cond.py
# This should pass.

from typing import Optional
class C:
    def __init__(self):
        self.field = 42
def f(x: Optional[C]):
    return x.field if x is not None else None