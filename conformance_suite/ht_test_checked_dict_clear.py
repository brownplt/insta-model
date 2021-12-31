# ht_test_checked_dict_clear.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict
x = CheckedDict[str, str]({"x": "abc"})
x.clear()
assert x == {}

# def test_checked_dict_clear(self):
#     x = chkdict[str, str](x="abc")
#     x.clear()
#     self.assertEqual(x, {})
