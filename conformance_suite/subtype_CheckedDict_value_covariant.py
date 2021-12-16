# subtype_CheckedDict_value_covariant.py
# This should fail.

from __static__ import CheckedDict

class C:
    pass

d: CheckedDict[str, object] = CheckedDict[str, C]({})
