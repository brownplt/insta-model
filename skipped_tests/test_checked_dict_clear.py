# Reason: Can't be translated by any of the three translator
def test_checked_dict_clear(self):
    x = chkdict[str, str](x="abc")
    x.clear()
    self.assertEqual(x, {})
