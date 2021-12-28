# Reason: Format too complicated
def test_checked_dict_update_bad_type(self):
    x = chkdict[str, int]()
    with self.assertRaises(TypeError):
        x.update(x="abc")
    self.assertEqual(x, {})
    with self.assertRaises(TypeError):
        x.update({"x": "abc"})
    with self.assertRaises(TypeError):
        x.update({24: 42})
    self.assertEqual(x, {})
