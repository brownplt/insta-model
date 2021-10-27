# test_assign_test_var.py
# This should pass.

from typing import Optional
def f(x: Optional[int]) -> int:
    if x is None:
        x = 1
    return x