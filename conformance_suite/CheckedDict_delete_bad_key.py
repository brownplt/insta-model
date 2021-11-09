# CheckedDict_delete_bad_key.py
# This should pass.
# This should error.

from __static__ import CheckedDict

x: CheckedDict[str, int] = CheckedDict[str, int]({"foo": 2, "bar": 3})
del x["other"]
