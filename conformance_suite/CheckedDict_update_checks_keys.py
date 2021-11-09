# CheckedDict_update_checks_keys.py
# This should pass.
# This should error.

from __static__ import CheckedDict

def asDyn(x):
    return x

x: CheckedDict[str, int] = CheckedDict[str, int]({"foo": 2})
asDyn(x)[42] = "bar"