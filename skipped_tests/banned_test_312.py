# Reason: Test hitted a banned word int64
def test_inline_primitive(self):
    codestr = """
        from __static__ import int64, cbool, inline
        @inline
        def x(i: int64) -> cbool:
            return i == 1
        def foo(i: int64) -> cbool:
            return i >0 and x(i)
    """
    with self.in_module(codestr, optimize=2) as mod:
        foo = mod.foo
        self.assertEqual(foo(0), False)
        self.assertEqual(foo(1), True)
        self.assertEqual(foo(2), False)
        self.assertNotInBytecode(foo, "STORE_FAST")
        self.assertInBytecode(foo, "STORE_LOCAL")
