def test_checked_dict_optional(self):
    x = chkdict[str, Optional[str]]()
    x["abc"] = None
    x = chkdict[Optional[str], str]()
    x[None] = "abc"
