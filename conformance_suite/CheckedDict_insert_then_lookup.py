# CheckedDict_insert_then_lookup.py
# This should pass.
# This should error.

from __static__ import CheckedDict

x: CheckedDict[str, int] = CheckedDict[str, int]({})
x["foo"] = 42
x["foo"]