def test_checked_dict_type_name(self):
    self.assertEqual(chkdict.__name__, "chkdict[K, V]")
    x = chkdict[str, str]
    self.assertEqual(x.__name__, "chkdict[str, str]")
