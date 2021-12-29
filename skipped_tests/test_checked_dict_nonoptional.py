# Reason: Can't be translated by any of the three translator
def test_checked_dict_nonoptional(self):
    x = chkdict[str, Optional[str]]()
    with self.assertRaises(TypeError):
        x[None] = "abc"
    x = chkdict[Optional[str], str]()
    with self.assertRaises(TypeError):
        x["abc"] = None
