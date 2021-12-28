# Reason: Test hitted a banned word int8
def test_conversion_narrow_primitive(self):
    codestr = f"""
        from __static__ import int64, Vector, uint8, unbox
        def f(i: int64):
            v = Vector[uint8]([0])
            v[0] = uint8(i if i != -1 else unbox(255))
            return v
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(list(f(42)), [42])
