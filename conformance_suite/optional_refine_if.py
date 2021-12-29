# optional_refine_if.py
# This should pass.
# This should terminate.

from typing import Optional

def f(x: Optional[int]) -> int:
    if x:
        return x
    else:
        return 42

assert f(2) is 2
assert f(None) is 42
