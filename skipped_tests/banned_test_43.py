# Reason: Test hitted a banned word box
def test_int_compare_and(self):
    codestr = """
    from __static__ import box, ssize_t
    def testfunc():
        i: ssize_t = 0
        j = i > 2 and i > 3
        return box(j)
    """
    with self.in_module(codestr) as mod:
        f = mod.testfunc
        self.assertInBytecode(f, "JUMP_IF_ZERO_OR_POP")
        self.assertIs(f(), False)
