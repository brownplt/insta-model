# Reason: Test hitted a banned word box
def test_int_compare_unboxed(self):
    codestr = f"""
    from __static__ import ssize_t, unbox
    def testfunc(x, y):
        x1: ssize_t = unbox(x)
        y1: ssize_t = unbox(y)
        if x1 > y1:
            return True
        return False
    """
    code = self.compile(codestr)
    f = self.run_code(codestr)["testfunc"]
    self.assertInBytecode(f, "POP_JUMP_IF_ZERO")
    self.assertEqual(f(1, 2), False)
