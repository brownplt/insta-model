# Reason: Format too complicated
def test_checked_dict_types_enforced(self):
    x = chkdict[str, str]()
    with self.assertRaises(TypeError):
        x[42] = "abc"
    self.assertEqual(x, {})
    with self.assertRaises(TypeError):
        x["abc"] = 42
    self.assertEqual(x, {})
    x = chkdict[str, int]()
    with self.assertRaises(TypeError):
        x[42] = 42
    self.assertEqual(x, {})
    with self.assertRaises(TypeError):
        x["abc"] = "abc"
    self.assertEqual(x, {})
