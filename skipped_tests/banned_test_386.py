# Reason: Test hitted a banned word int64
def test_no_cast_after_box(self):
    codestr = """
        from __static__ import int64, box
        def f(x: int) -> int:
            y = int64(x) + 1
            return box(y)
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertNotInBytecode(f, "CAST")
        self.assertInBytecode(f, "PRIMITIVE_LOAD_CONST", (1, TYPED_INT64))
        self.assertEqual(f(3), 4)
