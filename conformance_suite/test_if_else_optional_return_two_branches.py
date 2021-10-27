# test_if_else_optional_return_two_branches.py
# This should pass.

from typing import Optional
class C:
    def __init__(self):
        self.field = self
def f(x: Optional[C]):
    if x is None:
        if a:
            return 0
        else:
            return 2
    return x.field