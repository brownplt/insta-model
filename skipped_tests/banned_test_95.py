# Reason: Test hitted a banned word int64
def test_primitive_defaults_nested_func(self):
    code = """
        from __static__ import int64, box
        def g():
            def f(a: int64 = 42) -> int64:
                return a
            return f
    """
    with self.in_module(code) as mod:
        g = mod.g
        self.assertEqual(g()(), 42)
