# CheckedDict_lookup_cod.py
# This should fail.

from __static__ import PyDict, CheckedDict

x: CheckedDict[str, int] = CheckedDict[str, int]({"foo": 1})
y: str = x["foo"]
