# subtype_CheckedDict_value_contravariant.py
# This should fail.

from __static__ import CheckedDict

class C:
    pass

d: CheckedDict[str, C] = CheckedDict[str, object]({})
