# Reason: Can't be translated by any of the three translator
def test_checked_dict_bad_ctor(self):
    with self.assertRaises(TypeError):
        chkdict[str, str](None)
