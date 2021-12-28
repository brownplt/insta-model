# Reason: Test hitted a banned word int32
def test_inline_arg_type(self):
    codestr = """
        from __static__ import box, inline, int64, int32
        @inline
        def f(x: int64) -> int:
            return box(x)
        def g(arg: int) -> int:
            return f(int64(arg))
    """
    with self.in_module(codestr, optimize=2) as mod:
        g = mod.g
        self.assertInBytecode(g, "PRIMITIVE_BOX")
        self.assertEqual(g(3), 3)
