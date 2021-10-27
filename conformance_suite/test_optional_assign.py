# test_optional_assign.py
# This should pass.

from typing import Optional
class C:
    def f(self, x: Optional["C"]):
        if x is None:
            return self
        else:
            p: Optional["C"] = x