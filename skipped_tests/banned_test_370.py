# Reason: Test hitted a banned word int32
def test_inline_primitive_multiple(self):
    codestr = """
        from __static__ import cbool, inline, int64, int32
        @inline
        def f(x: int64) -> cbool:
            return x == 1
        @inline
        def g(x: int32) -> cbool:
            return x == 2
        def h(a: int64, b: int32) -> cbool:
            return f(a) and g(b)
    """
    with self.in_module(codestr, optimize=2) as mod:
        h = mod.h
        self.assertNotInBytecode(h, "INVOKE_FUNCTION")
        self.assertNotInBytecode(h, "CALL_FUNCTION")
        self.assertEqual(h(1, 2), True)
