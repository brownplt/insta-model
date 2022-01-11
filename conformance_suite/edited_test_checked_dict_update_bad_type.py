# ht_test_checked_dict_update_bad_type.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x = CheckedDict[str, int]()
try:
    x.update({"x": "abc"})
except TypeError:
    pass
else:
    raise Exception()

assert x == {}

try:
    x.update({"x": "abc"})
except TypeError:
    pass
else:
    raise Exception()

try:
    x.update({24: 42})
except TypeError:
    pass
else:
    raise Exception()

assert x == {}

# def test_checked_dict_update_bad_type(self):
#     x = chkdict[str, int]()
#     with self.assertRaises(TypeError):
#         x.update(x="abc")
#     self.assertEqual(x, {})
#     with self.assertRaises(TypeError):
#         x.update({"x": "abc"})
#     with self.assertRaises(TypeError):
#         x.update({24: 42})
#     self.assertEqual(x, {})
