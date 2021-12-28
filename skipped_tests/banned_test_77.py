# Reason: Test hitted a banned word int64
def test_uninit_value(self):
    codestr = """
    from __static__ import box, int64
    def f():
        x:int64
        return box(x)
        x = 0
    """
    f = self.run_code(codestr)["f"]
    self.assertEqual(f(), 0)
