# subtype_CheckedDict_key_covariant.py
# This should fail.

from __static__ import CheckedDict

class C:
    pass

d: CheckedDict[object, str] = CheckedDict[C, str]({})
