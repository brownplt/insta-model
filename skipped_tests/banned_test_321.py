# Reason: Test hitted a banned word _kw
def test_inline_kwarg(self):
    codestr = """
        from __static__ import inline
        @inline
        def f(x, y):
            return x + y
        def g():
            return f(x=1,y=2)
    """
    with self.in_module(codestr, optimize=2) as mod:
        g = mod.g
        self.assertInBytecode(g, "LOAD_CONST", 3)
        self.assertEqual(g(), 3)
