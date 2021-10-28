def test_checked_dict_copy(self):
    x = chkdict[str, str](x="abc")
    self.assertEqual(type(x), chkdict[str, str])
    self.assertEqual(x, {"x": "abc"})
