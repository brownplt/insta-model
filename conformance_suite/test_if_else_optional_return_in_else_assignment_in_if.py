# test_if_else_optional_return_in_else_assignment_in_if.py
# This should pass.

from typing import Optional
def f(x: Optional[int]) -> int:
    if x is None:
        x = 1
    else:
        return 2
    return x