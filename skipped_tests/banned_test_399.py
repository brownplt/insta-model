# Reason: Test hitted a banned word xxclassloader
def test_generic_type_inst_non_optional_error(self):
    from xxclassloader import spamobj
    o = spamobj[str]()
    with self.assertRaises(TypeError):
        o.setstate(None)
