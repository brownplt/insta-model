# Reason: Test hitted a banned word b"
def test_checked_dict_ctor(self):
    self.assertEqual(chkdict[str, str](x="abc"), {"x": "abc"})
    self.assertEqual(chkdict[str, int](x=42), {"x": 42})
    self.assertEqual(chkdict[str, str]({"x": "abc"}), {"x": "abc"})
    self.assertEqual(chkdict[str, str]([("a", "b")]), {"a": "b"})
    self.assertEqual(chkdict[str, str]([("a", "b")]), {"a": "b"})
    self.assertEqual(chkdict[str, str](chkdict[str, str](x="abc")), {"x": "abc"})
    self.assertEqual(chkdict[str, str](chkdict[str, object](x="abc")), {"x": "abc"})
    self.assertEqual(chkdict[str, str](UserDict(x="abc")), {"x": "abc"})
    self.assertEqual(chkdict[str, str](UserDict(x="abc"), x="foo"), {"x": "foo"})
