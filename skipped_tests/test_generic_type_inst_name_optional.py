def test_generic_type_inst_name_optional(self):
    from xxclassloader import spamobj
    self.assertEqual(spamobj[Optional[str]].__name__, "spamobj[Optional[str]]")
