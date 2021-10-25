# test_inline_arg_type_mismatch.py
# This should fail.

from __static__ import inline
@inline
def f(x: int) -> bool:
    return x == 1
def g(arg: str) -> bool:
    return f(arg)