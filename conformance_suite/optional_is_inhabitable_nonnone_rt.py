# optional_is_inhabitable_nonnone_rt.py
# This should pass.
# This should terminate.

from typing import Optional

def f():
    return 42

x: Optional[int] = f()
