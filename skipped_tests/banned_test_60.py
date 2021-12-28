# Reason: Test hitted a banned word double
def test_double_unbox_using_double(self):
    codestr = f"""
        from __static__ import double, box
        def f():
            x = 1.2
            y = double(x)
            return box(y + 1.0)
    """
    f = self.run_code(codestr)["f"]
    self.assertEqual(f(), 2.2)
