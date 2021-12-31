# ht_test_checked_dict_sizeof.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict
x = CheckedDict[str, int]({"x": 2}).__sizeof__()
assert type(x) == int

# def test_checked_dict_sizeof(self):
#     x = chkdict[str, int](x=2).__sizeof__()
#     self.assertEqual(type(x), int)
