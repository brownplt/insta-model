# Reason: Test hitted a banned word int64
def test_sign_extend(self):
    codestr = f"""
        from __static__ import int16, int64, box
        def testfunc():
            x: int16 = -40
            y: int64 = x
            return box(y)
        """
    code = self.compile(codestr)
    f = self.run_code(codestr)["testfunc"]
    self.assertEqual(f(), -40)
