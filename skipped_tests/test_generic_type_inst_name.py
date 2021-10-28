def test_generic_type_inst_name(self):
    from xxclassloader import spamobj
    self.assertEqual(spamobj[str].__name__, "spamobj[str]")
