# Reason: Format too complicated
def test_checked_dict(self):
    x = chkdict[str, str]()
    x["abc"] = "foo"
    self.assertEqual(repr(x), "{'abc': 'foo'}")
    x = chkdict[str, int]()
    x["abc"] = 42
    x = chkdict[int, str]()
    x[42] = "abc"
