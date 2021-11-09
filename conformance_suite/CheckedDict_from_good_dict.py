# CheckedDict_from_good_dict.py
# This should pass.
# This should terminate.
from __static__ import CheckedDict

x: CheckedDict[int, str] = CheckedDict[int, str]({2: 'a', 3: 'b'})