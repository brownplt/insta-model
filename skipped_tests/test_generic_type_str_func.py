def test_generic_type_str_func(self):
    from xxclassloader import spamobj
    o = spamobj[str]()
    o.setstr("abc")
    self.assertEqual(o.getstr(), "abc")
    with self.assertRaises(TypeError):
        o.setstr(42)
