# Reason: Test hitted a banned word int8
def test_mixed_binop_sign(self):
    """mixed signed/unsigned ops should be promoted to signed"""
    codestr = """
        from __static__ import int8, uint8, box
        def testfunc():
            x: uint8 = 42
            y: int8 = 2
            return box(x / y)
    """
    code = self.compile(codestr)
    f = self.find_code(code)
    self.assertInBytecode(f, "PRIMITIVE_BINARY_OP", PRIM_OP_DIV_INT)
    with self.in_module(codestr) as mod:
        f = mod.testfunc
        self.assertEqual(f(), 21)
    codestr = """
        from __static__ import int8, uint8, box
        def testfunc():
            x: int8 = 42
            y: uint8 = 2
            return box(x / y)
    """
    code = self.compile(codestr)
    f = self.find_code(code)
    self.assertInBytecode(f, "PRIMITIVE_BINARY_OP", PRIM_OP_DIV_INT)
    with self.in_module(codestr) as mod:
        f = mod.testfunc
        self.assertEqual(f(), 21)
