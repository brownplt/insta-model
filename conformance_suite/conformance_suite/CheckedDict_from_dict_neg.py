# CheckedDict_from_dict_neg.py
# This should fail.
from __static__ import CheckedDict

x: CheckedDict[int, str] = CheckedDict[int, str](42)