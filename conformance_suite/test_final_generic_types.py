# test_final_generic_types.py
# This should pass.

from typing import Final
def g(i: int) -> int:
    return i
def f() -> int:
    x: Final[int] = 0xdeadbeef
    return g(x)