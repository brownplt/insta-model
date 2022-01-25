# CheckedDict_update_dom_dyn_good.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

def asDyn(x):
    return x

x: CheckedDict[str, int] = CheckedDict[str, int]({"foo": 2})
asDyn(x)["bar"] = 3