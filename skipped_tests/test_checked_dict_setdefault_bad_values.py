# Reason: Format too complicated
def test_checked_dict_setdefault_bad_values(self):
    x = chkdict[str, int]()
    with self.assertRaises(TypeError):
        x.setdefault("abc", "abc")
    self.assertEqual(x, {})
    with self.assertRaises(TypeError):
        x.setdefault(42, 42)
    self.assertEqual(x, {})
