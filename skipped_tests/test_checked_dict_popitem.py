# Reason: Can't be translated by any of the three translator
def test_checked_dict_popitem(self):
    x = chkdict[str, int](x=2)
    y = x.popitem()
    self.assertEqual(y, ("x", 2))
    with self.assertRaises(KeyError):
        x.popitem()
