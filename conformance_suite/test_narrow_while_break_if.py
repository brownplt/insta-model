# test_narrow_while_break_if.py
# This should pass.

from typing import Optional
def f(x: Optional[int]) -> int:
    while True:
        if x is None:
            break
        return x