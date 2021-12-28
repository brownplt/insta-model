# Reason: Test hitted a banned word int64
def test_uninit_value_2(self):
    codestr = """
    from __static__ import box, int64
    def testfunc(x):
        if x:
            y:int64 = 42
        return box(y)
    """
    f = self.run_code(codestr)["testfunc"]
    self.assertEqual(f(False), 0)
