# PyDict_lookup_good_key.py
# This should pass.
# This should terminate.

from __static__ import PyDict

x: PyDict = {1: "foo", "bar": 2}
assert x["bar"] == 2
