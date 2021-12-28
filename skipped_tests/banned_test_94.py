# Reason: Test hitted a banned word int64
def test_primitive_defaults(self):
    code = """
        from __static__ import int64, box
        def f(a: int64 = 42) -> int64:
            return a
        def g():
            return box(f())
    """
    with self.in_module(code) as mod:
        g = mod.g
        f = mod.f
        self.assertEqual(g(), 42)
        self.assertEqual(f(), 42)
        self.assertEqual(f(0), 0)
