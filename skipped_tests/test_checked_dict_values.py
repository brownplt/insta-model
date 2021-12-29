# Reason: Can't be translated by any of the three translator
def test_checked_dict_values(self):
    x = chkdict[str, int](x=2, y=3)
    self.assertEqual(list(x.values()), [2, 3])
