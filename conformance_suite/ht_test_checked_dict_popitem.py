# ht_test_checked_dict_popitem.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x = CheckedDict[str, int]({"x": 2})
y = x.popitem()
assert y == ("x", 2)
try:
    x.popitem()
except KeyError:
    pass
else:
    raise Exception()

# def test_checked_dict_popitem(self):
#     x = chkdict[str, int](x=2)
#     y = x.popitem()
#     self.assertEqual(y, ("x", 2))
#     with self.assertRaises(KeyError):
#         x.popitem()
