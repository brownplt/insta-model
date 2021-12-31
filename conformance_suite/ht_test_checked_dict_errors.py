# ht_test_checked_dict_errors.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x = CheckedDict[str, int]({"x": 2})
try:
    x.get(100)
except TypeError:
    pass
else:
    raise Exception()

try:
    x.get("x", "abc")
except TypeError:
    pass
else:
    raise Exception()

# # Reason: Can't be translated by any of the three translator
# def test_checked_dict_errors(self):
#     x = chkdict[str, int](x=2)
#     with self.assertRaises(TypeError):
#         x.get(100)
#     with self.assertRaises(TypeError):
#         x.get("x", "abc")
