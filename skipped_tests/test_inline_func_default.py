def test_inline_func_default(self):
    codestr = """
        from __static__ import inline
        @inline
        def f(x, y = 2):
            return x + y
        def g():
            return f(1)
    """
    with self.in_module(codestr, optimize=2) as mod:
        g = mod.g
        self.assertInBytecode(g, "LOAD_CONST", 3)
        self.assertEqual(g(), 3)
