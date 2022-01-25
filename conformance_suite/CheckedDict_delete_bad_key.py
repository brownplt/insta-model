# CheckedDict_lookup_bad_key.py
# This should pass.
# This should terminate.

from __static__ import PyDict, CheckedDict

x: CheckedDict[str, int] = CheckedDict[str, int]({"foo": 1})
try:
    del x["bar"]
except KeyError:
    pass
else:
    raise Exception()
