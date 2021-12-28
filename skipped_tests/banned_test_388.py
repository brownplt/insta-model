# Reason: Test hitted a banned word int64
def test_rand_max_inlined(self):
    codestr = """
        from __static__ import rand, RAND_MAX, box, int64
        def f() -> int:
            x: int64 = rand() // int64(RAND_MAX)
            return box(x)
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "LOAD_CONST")
        self.assertInBytecode(f, "PRIMITIVE_UNBOX")
        self.assertIsInstance(f(), int)
