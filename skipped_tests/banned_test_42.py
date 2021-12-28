# Reason: Test hitted a banned word box
def test_int_compare_or(self):
    codestr = """
    from __static__ import box, ssize_t
    def testfunc():
        i: ssize_t = 0
        j = i > 2 or i < -2
        return box(j)
    """
    with self.in_module(codestr) as mod:
        f = mod.testfunc
        self.assertInBytecode(f, "JUMP_IF_NONZERO_OR_POP")
        self.assertIs(f(), False)
