# test_if_else_optional_return.py
# This should pass.

from typing import Optional
class C:
    def __init__(self):
        self.field = self
def f(x: Optional[C]):
    if x is None:
        return 0
    return x.field