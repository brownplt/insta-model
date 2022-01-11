# ht_test_checked_dict_values.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x = CheckedDict[str, int]({"x": 2, "y": 3})
assert list(x.values()) == [2, 3]

# def test_checked_dict_values(self):
#     x = chkdict[str, int](x=2, y=3)
#     self.assertEqual(list(x.values()), [2, 3])
