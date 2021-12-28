# Reason: Test hitted a banned word int64
def test_primitive_compare_returns_cbool(self):
    codestr = """
        from __static__ import cbool, int64
        def f(x: int64, y: int64) -> cbool:
            return x == y
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertIs(f(1, 1), True)
        self.assertIs(f(1, 2), False)
