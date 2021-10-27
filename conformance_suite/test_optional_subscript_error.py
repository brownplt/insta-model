# test_optional_subscript_error.py
# This should fail.

from typing import Optional
def f(a: Optional[int]):
    a[1]