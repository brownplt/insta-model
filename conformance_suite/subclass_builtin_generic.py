# subclass_builtin_generic.py
# This should pass.

from typing import CheckedDict

class C(CheckedDict[str, int]):
    pass

x: C = C({})
y: CheckedDict[str, int] = x
