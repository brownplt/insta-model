# CheckedDict_update.py
# This should pass.
# This should terminate.

from __static__ import PyDict, CheckedDict

x: CheckedDict[str, int] = CheckedDict[str, int]({"foo": 1})

x["bar"] = 2
y = x["bar"]
assert y == 2

x["foo"] = 3
y = x["foo"]
assert y == 3
