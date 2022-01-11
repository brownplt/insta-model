# ht_test_checked_dict_fromkeys_bad_types.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

try:
    CheckedDict[str, int].fromkeys([2], 42)
except TypeError:
    pass
else:
    raise Exception()

try:
    CheckedDict[str, int].fromkeys("abc", object())
except TypeError:
    pass
else:
    raise Exception()

try:
    CheckedDict[str, int].fromkeys("abc")
except TypeError:
    pass
else:
    raise Exception()

# def test_checked_dict_fromkeys_bad_types(self):
#     with self.assertRaises(TypeError):
#         chkdict[str, int].fromkeys([2], 42)
#     with self.assertRaises(TypeError):
#         chkdict[str, int].fromkeys("abc", object())
#     with self.assertRaises(TypeError):
#         chkdict[str, int].fromkeys("abc")

