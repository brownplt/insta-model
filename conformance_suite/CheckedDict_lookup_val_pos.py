# CheckedDict_lookup_val_pos.py
# This should pass.

from __static__ import PyDict, CheckedDict

x: CheckedDict[str, int] = CheckedDict[str, int]({"foo": 1})
y: int = x["foo"]