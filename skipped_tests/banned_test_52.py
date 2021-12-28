# Reason: Test hitted a banned word int8
def test_int_binop_type_context(self):
    codestr = f"""
        from __static__ import box, int8, int16
        def f(x: int8, y: int8) -> int:
            z: int16 = x * y
            return box(z)
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(
            f, "CONVERT_PRIMITIVE", TYPED_INT8 | (TYPED_INT16 << 4)
        )
        self.assertEqual(f(120, 120), 14400)
