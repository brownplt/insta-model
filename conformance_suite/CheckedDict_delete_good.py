# CheckedDict_delete_good.py
# This should pass.
# This should terminate.

from __static__ import PyDict, CheckedDict

x: CheckedDict[str, int] = CheckedDict[str, int]({"foo": 1})
y: int = x["foo"]

del x["foo"]

try:
    x["foo"]
except KeyError:
    pass
else:
    raise Exception()
