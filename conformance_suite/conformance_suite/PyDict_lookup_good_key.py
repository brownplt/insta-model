# PyDict_lookup_good_key.py
# This should pass.

from __static__ import PyDict

x: PyDict = {1: "foo", "bar": 2}
x["bar"]
