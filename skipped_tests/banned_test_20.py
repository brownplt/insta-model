# Reason: Test hitted a banned word int8
def test_mixed_add_reversed(self):
    codestr = """
        from __static__ import int8, uint8, int64, box, int16
        def testfunc(tst=False):
            x: int8 = 42
            y: int16 = 2
            if tst:
                x += 1
                y += 1
            return box(y + x)
    """
    code = self.compile(codestr)
    f = self.find_code(code)
    self.assertInBytecode(f, "PRIMITIVE_BINARY_OP", PRIM_OP_ADD_INT)
    with self.in_module(codestr) as mod:
        f = mod.testfunc
        self.assertEqual(f(), 44)
