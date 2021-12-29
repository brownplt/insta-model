# optional_refine_is_None.py
# This should pass.
# This should terminate.

from typing import Optional

def f(x: Optional[int]) -> int:
    if x is None:
        return 42
    else:
        return x

assert f(2) is 2
assert f(None) is 42
