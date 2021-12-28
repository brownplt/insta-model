# Reason: Test hitted a banned word double
def test_double_unbox(self):
    codestr = f"""
    from __static__ import double, box, unbox
    def fn(x, y):
        a: double = unbox(x)
        b: double = unbox(y)
        return box(a + b)
    """
    f = self.run_code(codestr)["fn"]
    x = 3.14
    y = 1.732
    self.assertEqual(f(x, y), x + y)
