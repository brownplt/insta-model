# Reason: Test hitted a banned word box
def test_int_assert(self):
    codestr = """
    from __static__ import ssize_t, box
    def testfunc():
        i: ssize_t = 0
        assert i == 0, "hello there"
    """
    code = self.compile(codestr)
    f = self.find_code(code)
    self.assertInBytecode(f, "POP_JUMP_IF_NONZERO")
    f = self.run_code(codestr)["testfunc"]
    self.assertEqual(f(), None)
