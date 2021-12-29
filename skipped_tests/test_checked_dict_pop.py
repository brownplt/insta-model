# Reason: Can't be translated by any of the three translator
def test_checked_dict_pop(self):
    x = chkdict[str, int](x=2)
    y = x.pop("x")
    self.assertEqual(y, 2)
    with self.assertRaises(KeyError):
        x.pop("z")
