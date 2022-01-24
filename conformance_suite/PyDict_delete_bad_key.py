# PyDict_delete_bad_key.py
# This should pass.
# This should terminate.

from __static__ import PyDict

x: PyDict = {1: "foo", "bar": 2}
try:
    del x["other"]
except KeyError:
    pass
else:
    raise Exception()
