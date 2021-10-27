# test_optional_unary_error.py
# This should fail.

from typing import Optional
def f(a: Optional[int]):
    -a