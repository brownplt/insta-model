# Reason: Test hitted a banned word int8
def test_generic_int_funcs_overflow(self):
    from xxclassloader import spamobj
    o = spamobj[str]()
    o.setuint64(42)
    for i, f in enumerate([o.setint8, o.setint16, o.setint32, o.setint]):
        with self.assertRaises(OverflowError):
            x = -(1 << ((8 << i) - 1)) - 1
            f(x)
        with self.assertRaises(OverflowError):
            x = 1 << ((8 << i) - 1)
            f(x)
