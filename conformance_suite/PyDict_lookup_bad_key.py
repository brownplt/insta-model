# PyDict_lookup_bad_key.py
# This should pass.
# This should error.

from __static__ import PyDict

x: PyDict = {1: "foo", "bar": 2}
x["other"]
