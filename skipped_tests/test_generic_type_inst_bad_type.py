def test_generic_type_inst_bad_type(self):
    from xxclassloader import spamobj
    o = spamobj[str]()
    with self.assertRaises(TypeError):
        o.setstate(42)
