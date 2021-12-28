# Reason: Test hitted a banned word int64
def test_int_unbox_with_conversion(self):
    codestr = """
        from __static__ import int64
        def f(x) -> int64:
            return int64(int(x))
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(f(42.0), 42)
