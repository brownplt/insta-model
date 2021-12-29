# union_optional_is_supported_pos.py
# This should pass.
# This should terminate.

from typing import Union

def f(x: Union[None, int]):
    pass

f(None)
f(42)
