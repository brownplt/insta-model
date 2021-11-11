# Reason: Format too complicated
def test_checked_dict_free_list(self):
    t1 = chkdict[str, int]
    t2 = chkdict[str, str]
    x = t1()
    x_id1 = id(x)
    del x
    x = t2()
    x_id2 = id(x)
    self.assertEqual(x_id1, x_id2)
