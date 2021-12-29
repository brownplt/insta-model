# Reason: Can't be translated by any of the three translator
def test_checked_dict_setdefault(self):
    x = chkdict[str, str]()
    x.setdefault("abc", "foo")
    self.assertEqual(x, {"abc": "foo"})
