# CheckedDict_lookup_checks_dynamically_fail.py
# This should pass.
# This should error.

from __static__ import CheckedDict

def asDyn(x):
    return x

x: CheckedDict[str, int] = CheckedDict[str, int]({"foo": 2})
del asDyn(x)[42]