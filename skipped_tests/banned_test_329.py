# Reason: Test hitted a banned word int32
def test_chained_compare_primitive_mixed(self):
    for jumpif in [False, True]:
        with self.subTest(jumpif=jumpif):
            if jumpif:
                pre = ""
                test = "a < x < b"
            else:
                pre = "y = a < x < b"
                test = "y"
            codestr = f"""
                from __static__ import int16, int32, int64
                def f(x: int16):
                    a: int32 = 1
                    b: int64 = 5
                    if x:
                        a += 1
                        b += 1
                    {pre}
                    if {test}:
                        return 1
                    return 0
            """
            with self.in_module(codestr) as mod:
                f = mod.f
                self.assertInBytecode(
                    f, "CONVERT_PRIMITIVE", TYPED_INT16 | (TYPED_INT32 << 4)
                )
                self.assertInBytecode(
                    f, "CONVERT_PRIMITIVE", TYPED_INT16 | (TYPED_INT64 << 4)
                )
                self.assertEqual(f(2), 0)
                self.assertEqual(f(3), 1)
                self.assertEqual(f(5), 1)
                self.assertEqual(f(6), 0)
