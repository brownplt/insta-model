# Reason: Test hitted a banned word test_inline_bare_return
def test_inline_bare_return(self):
    codestr = """
        from __static__ import inline
        @inline
        def f(x, y):
            return
        def g():
            return f(x=1,y=2)
    """
    with self.in_module(codestr, optimize=2) as mod:
        g = mod.g
        self.assertInBytecode(g, "LOAD_CONST", None)
        self.assertEqual(g(), None)
