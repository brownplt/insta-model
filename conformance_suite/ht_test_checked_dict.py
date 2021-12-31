# ht_test_checked_dict.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x = CheckedDict[str, str]()
x["abc"] = "foo"
x = CheckedDict[str, int]()
x["abc"] = 42
x = CheckedDict[int, str]()
x[42] = "abc"

# def test_checked_dict(self):
#     x = chkdict[str, str]()
#     x["abc"] = "foo"
#     self.assertEqual(repr(x), "{'abc': 'foo'}")
#     x = chkdict[str, int]()
#     x["abc"] = 42
#     x = chkdict[int, str]()
#     x[42] = "abc"
