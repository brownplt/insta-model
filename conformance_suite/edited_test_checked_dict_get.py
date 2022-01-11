# ht_test_checked_dict_get.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x = CheckedDict[str, int]({"x": 2})
assert x.get("x") == 2
assert x.get("y", 100) == 100


# def test_checked_dict_get(self):
#     x = chkdict[str, int](x=2)
#     self.assertEqual(x.get("x"), 2)
#     self.assertEqual(x.get("y", 100), 100)
