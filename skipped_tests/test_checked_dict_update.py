# Reason: Can't be translated by any of the three translator
def test_checked_dict_update(self):
    x = chkdict[str, str](x="abc")
    x.update(y="foo")
    self.assertEqual(x, {"x": "abc", "y": "foo"})
    x.update({"z": "bar"})
    self.assertEqual(x, {"x": "abc", "y": "foo", "z": "bar"})
