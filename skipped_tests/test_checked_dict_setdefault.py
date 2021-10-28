def test_checked_dict_setdefault(self):
    x = chkdict[str, str]()
    x.setdefault("abc", "foo")
    self.assertEqual(x, {"abc": "foo"})
