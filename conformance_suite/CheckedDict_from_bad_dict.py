# CheckedDict_from_bad_dict.py
# This should pass.
# This should error.
from __static__ import CheckedDict

x: CheckedDict[int, str] = CheckedDict[int, str]({2: 'a', 3: 4})