# Reason: Format too complicated
def test_checked_dict_fromkeys_bad_types(self):
    with self.assertRaises(TypeError):
        chkdict[str, int].fromkeys([2], 42)
    with self.assertRaises(TypeError):
        chkdict[str, int].fromkeys("abc", object())
    with self.assertRaises(TypeError):
        chkdict[str, int].fromkeys("abc")
