# Reason: Test hitted a banned word box
def test_int_compare_mixed(self):
    codestr = """
    from __static__ import box, ssize_t
    x = 1
    def testfunc():
        i: ssize_t = 0
        j = 0
        while box(i < 100) and x:
            i = i + 1
            j = j + 1
        return j
    """
    code = self.compile(codestr)
    f = self.run_code(codestr)["testfunc"]
    self.assertEqual(f(), 100)
    self.assert_jitted(f)
