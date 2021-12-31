# ht_test_checked_dict_update.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict

x = CheckedDict[str, str]({"x": "abc"})
x.update({"y": "foo"})
assert x == {"x": "abc", "y": "foo"}
x.update({"z": "bar"})
assert x == {"x": "abc", "y": "foo", "z": "bar"}

# def test_checked_dict_update(self):
#     x = chkdict[str, str](x="abc")
#     x.update(y="foo")
#     self.assertEqual(x, {"x": "abc", "y": "foo"})
#     x.update({"z": "bar"})
#     self.assertEqual(x, {"x": "abc", "y": "foo", "z": "bar"})
