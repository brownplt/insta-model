# optional_refine_or.py
# This should fail.

from typing import Optional

def expect_int(i: int) -> int:
    return 42

def f(x: Optional[int]) -> int:
    return x or expect_int(x)
