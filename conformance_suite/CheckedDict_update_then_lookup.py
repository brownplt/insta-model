# CheckedDict_update_then_lookup.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x: CheckedDict[str, int] = CheckedDict[str, int]({"foo": 2})
x["foo"] = 3
assert x["foo"] is 3