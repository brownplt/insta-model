# ht_test_checked_dict_copy.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x = CheckedDict[str, str]({ "x": "abc" })
assert type(x) == CheckedDict[str, str]
assert x == {"x": "abc"}

# def test_checked_dict_copy(self):
#     x = chkdict[str, str](x="abc")
#     self.assertEqual(type(x), chkdict[str, str])
#     self.assertEqual(x, {"x": "abc"})
