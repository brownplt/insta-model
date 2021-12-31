# ht_test_checked_dict_setdefault.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x = CheckedDict[str, str]()
x.setdefault("abc", "foo")
assert x == {"abc": "foo"}

# def test_checked_dict_setdefault(self):
#     x = chkdict[str, str]()
#     x.setdefault("abc", "foo")
#     self.assertEqual(x, {"abc": "foo"})
