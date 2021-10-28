def test_generic_type_inst_non_optional_error_func(self):
    from xxclassloader import spamobj
    o = spamobj[str]()
    f = o.setstate
    with self.assertRaises(TypeError):
        f(None)
