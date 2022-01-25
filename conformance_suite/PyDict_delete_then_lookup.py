# PyDict_delete_then_lookup.py
# This should pass.
# This should terminate.

from __static__ import PyDict

x: PyDict = {1: "foo", "bar": 2}
del x["bar"]
try:
    x["bar"]
except KeyError:
    pass
else:
    raise Exception()
