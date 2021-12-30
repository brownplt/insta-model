# CheckedDict_from_dict_literal_neg.py
# This should fail.

from __static__ import CheckedDict

x: CheckedDict[int, str] = {2: 3}
