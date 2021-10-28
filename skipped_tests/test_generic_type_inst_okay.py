def test_generic_type_inst_okay(self):
    from xxclassloader import spamobj
    o = spamobj[str]()
    o.setstate("abc")
