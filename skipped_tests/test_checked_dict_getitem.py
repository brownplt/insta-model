# Reason: Can't be translated by any of the three translator
def test_checked_dict_getitem(self):
    x = chkdict[str, int](x=2)
    self.assertEqual(x.__getitem__("x"), 2)
