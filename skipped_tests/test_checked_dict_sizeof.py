def test_checked_dict_sizeof(self):
    x = chkdict[str, int](x=2).__sizeof__()
    self.assertEqual(type(x), int)
