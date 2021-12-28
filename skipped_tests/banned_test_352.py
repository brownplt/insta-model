# Reason: Test hitted a banned word xxclassloader
def test_generic_type_bad_arg_cnt(self):
    from xxclassloader import spamobj
    o = spamobj[str]()
    with self.assertRaises(TypeError):
        o.setstr()
    with self.assertRaises(TypeError):
        o.setstr("abc", "abc")
