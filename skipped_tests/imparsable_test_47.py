# Reason: Format too complicated
def test_checked_dict_keys(self):
    x = chkdict[str, int](x=2)
    self.assertEqual(list(x.keys()), ["x"])
    x = chkdict[str, int](x=2, y=3)
    self.assertEqual(list(x.keys()), ["x", "y"])
