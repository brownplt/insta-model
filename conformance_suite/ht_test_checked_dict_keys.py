# ht_test_checked_dict_keys.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x = CheckedDict[str, int]({ "x": 2 })
assert list(x.keys()) == ["x"]
x = CheckedDict[str, int]({ "x": 2, "y": 3})
assert list(x.keys()) == ["x", "y"]

# def test_checked_dict_keys(self):
#     x = chkdict[str, int](x=2)
#     self.assertEqual(list(x.keys()), ["x"])
#     x = chkdict[str, int](x=2, y=3)
#     self.assertEqual(list(x.keys()), ["x", "y"])
