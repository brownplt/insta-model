# Reason: Hitted a skipped word (Final[)
def test_inline_final(self):
    codestr = """
        from __static__ import inline
        from typing import Final
        Y: Final[int] = 42
        @inline
        def f(x):
            return x + Y
        def g():
            return f(1)
    """
    with self.in_module(codestr, optimize=2) as mod:
        g = mod.g
        # We don't currently inline math with finals
        self.assertInBytecode(g, "LOAD_CONST", 42)
        self.assertEqual(g(), 43)
