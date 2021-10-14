# PyDict_delete.py
# This should pass.

from __static__ import PyDict

x: PyDict = {1: "foo", "bar": 2}
del x["bar"]
