# Reason: Format too complicated
def test_checked_dict_get(self):
    x = chkdict[str, int](x=2)
    self.assertEqual(x.get("x"), 2)
    self.assertEqual(x.get("y", 100), 100)
