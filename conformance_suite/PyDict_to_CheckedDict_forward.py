# PyDict_to_CheckedDict_forward.py
# This should pass.
# This should error.

from __static__ import CheckedDict

def f():
    return {}

x: CheckedDict[int, int] = f()
