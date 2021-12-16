# subtype_CheckedDict_key_contravariant.py
# This should fail.

from __static__ import CheckedDict

class C:
    pass

d: CheckedDict[C, str] = CheckedDict[object, str]({})
