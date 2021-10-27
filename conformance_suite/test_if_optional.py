# test_if_optional.py
# This should pass.

from typing import Optional
class C:
    def __init__(self):
        self.field = 42
def f(x: Optional[C]):
    if x is not None:
        return x.field
    return None