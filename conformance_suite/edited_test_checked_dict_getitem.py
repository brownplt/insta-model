# ht_test_checked_dict_getitem.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x = CheckedDict[str, int]({"x": 2})
assert x.__getitem__("x") == 2

# def test_checked_dict_getitem(self):
#     x = chkdict[str, int](x=2)
#     self.assertEqual(x.__getitem__("x"), 2)
