# CheckedDict_from_dict_literal_pos.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x: CheckedDict[int, str] = {2: 'a'}
