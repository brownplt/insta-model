# CheckedDict_lookup_checks_dynamically_pass.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

def asDyn(x):
    return x

x: CheckedDict[str, int] = CheckedDict[str, int]({"foo": 2})
asDyn(x)["foo"]