# PyDict_to_CheckedDict_backward.py
# This should pass.
# This should error.

from __static__ import CheckedDict, PyDict

def f():
    return CheckedDict[int, int]({})

x: PyDict = f()
