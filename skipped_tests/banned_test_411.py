# Reason: Test hitted a banned word xxclassloader
def test_generic_type_int_func(self):
    from xxclassloader import spamobj
    o = spamobj[str]()
    o.setint(42)
    self.assertEqual(o.getint(), 42)
    with self.assertRaises(TypeError):
        o.setint("abc")
