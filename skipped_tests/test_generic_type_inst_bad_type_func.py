# Reason: Hitted a skipped word (xxclassloader)
def test_generic_type_inst_bad_type_func(self):
    from xxclassloader import spamobj
    o = spamobj[str]()
    f = o.setstate
    with self.assertRaises(TypeError):
        f(42)
