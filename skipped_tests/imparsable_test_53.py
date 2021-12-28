# Reason: Format too complicated
def test_checked_dict_errors(self):
    x = chkdict[str, int](x=2)
    with self.assertRaises(TypeError):
        x.get(100)
    with self.assertRaises(TypeError):
        x.get("x", "abc")
