# Reason: Test hitted a banned word int8
def test_generic_int_funcs(self):
    from xxclassloader import spamobj
    o = spamobj[str]()
    o.setint(42)
    self.assertEqual(o.getint8(), 42)
    self.assertEqual(o.getint16(), 42)
    self.assertEqual(o.getint32(), 42)
