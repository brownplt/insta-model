# optional_is_inhabitable_other_rt.py
# This should pass.
# This should error.

from typing import Optional

def f():
    return "foo"

x: Optional[int] = f()
