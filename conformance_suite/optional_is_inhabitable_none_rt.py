# optional_is_inhabitable_none.py
# This should pass.
# This should terminate.

from typing import Optional

def f():
    return None

x: Optional[int] = f()
