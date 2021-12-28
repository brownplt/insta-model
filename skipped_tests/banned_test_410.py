# Reason: Test hitted a banned word int8
def test_generic_uint_funcs_overflow(self):
    from xxclassloader import spamobj
    o = spamobj[str]()
    o.setuint64(42)
    for f in [o.setuint8, o.setuint16, o.setuint32, o.setuint64]:
        with self.assertRaises(OverflowError):
            f(-1)
    for i, f in enumerate([o.setuint8, o.setuint16, o.setuint32, o.setuint64]):
        with self.assertRaises(OverflowError):
            x = (1 << (8 << i)) + 1
            f(x)
