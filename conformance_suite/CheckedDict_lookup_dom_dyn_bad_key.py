# CheckedDict_lookup_dom_dyn_bad_key.py
# This should pass.
# This should error.

from __static__ import CheckedDict

def asDyn(x):
    return x

x: CheckedDict[str, int] = CheckedDict[str, int]({"foo": 2})
asDyn(x)[42]