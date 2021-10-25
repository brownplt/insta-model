# test_bool_cast.py
# This should pass.

from __static__ import cast
class D: pass
def f(x) -> bool:
    y: bool = cast(bool, x)
    return y