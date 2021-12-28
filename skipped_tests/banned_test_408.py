# Reason: Test hitted a banned word int8
def test_generic_uint_funcs(self):
    from xxclassloader import spamobj
    o = spamobj[str]()
    o.setuint64(42)
    self.assertEqual(o.getuint8(), 42)
    self.assertEqual(o.getuint16(), 42)
    self.assertEqual(o.getuint32(), 42)
    self.assertEqual(o.getuint64(), 42)
