# ht_test_checked_dict_types_enforced.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x = CheckedDict[str, str]()
try:
    x[42] = "abc"
except TypeError:
    pass
else:
    raise Exception()
assert x == {}

try:
    x["abc"] = 42
except TypeError:
    pass
else:
    raise Exception()
assert x == {}

x = chkdict[str, int]()
try:
    x[42] = 42
except TypeError:
    pass
else:
    raise Exception()
assert x == {}

try:
    x["abc"] = "abc"
except TypeError:
    pass
else:
    raise Exception()
assert x == {}

# def test_checked_dict_types_enforced(self):
#     x = chkdict[str, str]()
#     with self.assertRaises(TypeError):
#         x[42] = "abc"
#     self.assertEqual(x, {})
#     with self.assertRaises(TypeError):
#         x["abc"] = 42
#     self.assertEqual(x, {})
#     x = chkdict[str, int]()
#     with self.assertRaises(TypeError):
#         x[42] = 42
#     self.assertEqual(x, {})
#     with self.assertRaises(TypeError):
#         x["abc"] = "abc"
#     self.assertEqual(x, {})
