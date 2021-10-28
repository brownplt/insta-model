def test_checked_dict_fromkeys_optional(self):
    x = chkdict[Optional[str], int].fromkeys(["a", "b", "c", None], 42)
    self.assertEqual(x, {"a": 42, "b": 42, "c": 42, None: 42})
    x = chkdict[str, Optional[int]].fromkeys("abc", None)
    self.assertEqual(x, {"a": None, "b": None, "c": None})
