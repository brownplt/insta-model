# test_assign_generic_optional_2.py
# This should fail.

from typing import Optional
def f():
    x: Optional = 42 + 1