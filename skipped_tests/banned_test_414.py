# Reason: Test hitted a banned word xxclassloader
def test_generic_type_bad_arg_cnt(self):
    from xxclassloader import spamobj
    o = spamobj[str]()
    self.assertEqual(o.twoargs(1, 2), 3)
