# CheckedDict_from_dict_pos.py
# This should pass.
from __static__ import CheckedDict

x: CheckedDict[int, str] = CheckedDict[int, str]({2: 'a', 3: 'b'})