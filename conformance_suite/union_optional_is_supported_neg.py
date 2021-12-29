# union_optional_is_supported_neg.py
# This should fail.

from typing import Union

def f(x: Union[None, int]):
    pass

f("foo")
