# PyDict_insert.py
# This should pass.
# This should terminate.

from __static__ import PyDict

x: PyDict = {1: "foo", "bar": 2}
x["new"] = "hello"
