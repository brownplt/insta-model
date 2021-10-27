# test_if_else_optional_return_in_else.py
# This should pass.

from typing import Optional
def f(x: Optional[int]) -> int:
    if x is not None:
        pass
    else:
        return 2
    return x