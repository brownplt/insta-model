# ht_test_checked_dict_setdefault_bad_values.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

def as_dyn(x):
    return x

x = CheckedDict[str, int]()
try:
    x.setdefault("abc", as_dyn("abc"))
except TypeError:
    pass
else:
    raise Exception()

assert x == {}
try:
    x.setdefault(as_dyn(42), 42)
except TypeError:
    pass
else:
    raise Exception()

assert x == {}

# def test_checked_dict_setdefault_bad_values(self):
#     x = chkdict[str, int]()
#     with self.assertRaises(TypeError):
#         x.setdefault("abc", "abc")
#     self.assertEqual(x, {})
#     with self.assertRaises(TypeError):
#         x.setdefault(42, 42)
#     self.assertEqual(x, {})
