def test_generic_type_def_bad_args(self):
    from xxclassloader import spamobj
    with self.assertRaises(TypeError):
        spamobj[str, int]
