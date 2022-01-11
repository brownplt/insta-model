# ht_test_checked_dict_items.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x = CheckedDict[str, int]({"x": 2})
assert list(x.items()) == [("x", 2)]
x = CheckedDict[str, int]({"x": 2, "y": 3})
assert list(x.items()) == [("x", 2), ("y", 3)]

# def test_checked_dict_items(self):
#     x = chkdict[str, int](x=2)
#     self.assertEqual(
#         list(x.items()),
#         [
#             ("x", 2),
#         ],
#     )
#     x = chkdict[str, int](x=2, y=3)
#     self.assertEqual(list(x.items()), [("x", 2), ("y", 3)])
