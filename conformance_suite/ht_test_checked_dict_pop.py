# ht_test_checked_dict_pop.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x = CheckedDict[str, int]({"x": 2})
y = x.pop("x")
assert y == 2
try:
    x.pop("z")
except KeyError:
    pass
else:
    raise Exception()

# def test_checked_dict_pop(self):
#     x = chkdict[str, int](x=2)
#     y = x.pop("x")
#     self.assertEqual(y, 2)
#     with self.assertRaises(KeyError):
#         x.pop("z")
