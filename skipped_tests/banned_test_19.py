# Reason: Test hitted a banned word int8
def test_mixed_cmpop_sign(self):
    """mixed signed/unsigned ops should be promoted to signed"""
    codestr = """
        from __static__ import int8, uint8, box
        def testfunc(tst=False):
            x: uint8 = 42
            y: int8 = 2
            if tst:
                x += 1
                y += 1
            if x < y:
                return True
            return False
    """
    code = self.compile(codestr)
    f = self.find_code(code)
    self.assertInBytecode(f, "PRIMITIVE_COMPARE_OP", PRIM_OP_LT_INT)
    with self.in_module(codestr) as mod:
        f = mod.testfunc
        self.assertEqual(f(), False)
    codestr = """
        from __static__ import int8, uint8, box
        def testfunc(tst=False):
            x: int8 = 42
            y: uint8 = 2
            if tst:
                x += 1
                y += 1
            if x < y:
                return True
            return False
    """
    code = self.compile(codestr)
    f = self.find_code(code)
    self.assertInBytecode(f, "PRIMITIVE_COMPARE_OP", PRIM_OP_LT_INT)
    with self.in_module(codestr) as mod:
        f = mod.testfunc
        self.assertEqual(f(), False)
