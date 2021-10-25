# test_cast_unknown_type.py
# This should fail.

from __static__ import cast
def f():
    cast(abc, 42)