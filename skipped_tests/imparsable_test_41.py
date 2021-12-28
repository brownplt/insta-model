# Reason: Format too complicated
def test_checked_dict_getitem(self):
    x = chkdict[str, int](x=2)
    self.assertEqual(x.__getitem__("x"), 2)
