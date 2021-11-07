# PyDict_update_then_lookup.py
# This should pass.
# This should terminate.

from __static__ import PyDict

x: PyDict = {"foo": 2}
x["foo"] = 3
assert x["foo"] is 3