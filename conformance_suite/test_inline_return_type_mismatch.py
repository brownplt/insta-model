# test_inline_return_type_mismatch.py
# This should fail.

from __static__ import inline
@inline
def f() -> int:
    return 1
def g() -> str:
    return f()