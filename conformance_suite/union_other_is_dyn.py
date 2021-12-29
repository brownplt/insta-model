# union_other_is_dyn.py
# This should pass.
# This should terminate.

from typing import Union

def f(x: Union[str, int]):
    pass

class C:
    pass

f(C())
