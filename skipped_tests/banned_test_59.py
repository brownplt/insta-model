# Reason: Test hitted a banned word box
def test_int_loop_inplace(self):
    codestr = """
    from __static__ import ssize_t, box
    def f():
        i: ssize_t = 0
        while i < 100:
            i += 1
        return box(i)
    """
    code = self.compile(codestr)
    f = self.find_code(code)
    f = self.run_code(codestr)["f"]
    self.assertEqual(f(), 100)
