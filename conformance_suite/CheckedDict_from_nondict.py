# CheckedDict_from_nondict.py
# This should pass.
# This should error.
from __static__ import CheckedDict

x: CheckedDict[int, str] = CheckedDict[int, str](42)