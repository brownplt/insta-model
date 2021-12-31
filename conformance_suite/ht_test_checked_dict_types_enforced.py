# ht_test_checked_dict_types_enforced.py
# This should pass.
# This should terminate.

from typing import Any
from __static__ import CheckedDict

def as_dyn(x):
    return x

x: Any = CheckedDict[str, str]()
try:
    x[as_dyn(42)] = "abc"
except TypeError:
    pass
else:
    raise Exception()
assert x == {}

try:
    x["abc"] = as_dyn(42)
except TypeError:
    pass
else:
    raise Exception()
assert x == {}

x = CheckedDict[str, int]()
try:
    x[as_dyn(42)] = 42
except TypeError:
    pass
else:
    raise Exception()
assert x == {}

try:
    x["abc"] = as_dyn("abc")
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
