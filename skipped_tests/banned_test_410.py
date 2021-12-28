# Reason: Test hitted a banned word b"
def test_checked_dict_fromkeys(self):
    x = chkdict[str, int].fromkeys("abc", 42)
    self.assertEqual(x, {"a": 42, "b": 42, "c": 42})
