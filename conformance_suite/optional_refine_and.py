# optional_refine_and.py
# This should pass.
# This should terminate.

from typing import Optional

def expect_int(i: int) -> None:
    return

def f(x: Optional[int]) -> None:
    return x and expect_int(x)

assert f(None) is None
assert f(42) is None
