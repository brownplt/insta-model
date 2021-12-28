# Reason: Test hitted a banned word int8
def test_mixed_assign_larger_2(self):
    """promote int/uint to int16"""
    codestr = """
        from __static__ import int8, uint8, int16, box
        def testfunc(tst=False):
            x: uint8 = 42
            y: int8 = 2
            z: int16
            z = x + y
            return box(z)
    """
    code = self.compile(codestr)
    f = self.find_code(code)
    self.assertInBytecode(f, "PRIMITIVE_BINARY_OP", PRIM_OP_ADD_INT)
    with self.in_module(codestr) as mod:
        f = mod.testfunc
        self.assertEqual(f(), 44)
