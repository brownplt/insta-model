# Reason: Test hitted a banned word double
def test_double_unary(self):
    tests = [
        ("-", 1.0, -1.0),
        ("+", 1.0, 1.0),
        ("-", -1.0, 1.0),
    ]
    for op, x, res in tests:
        codestr = f"""
        from __static__ import double, box
        def testfunc(tst):
            x: double = {x}
            if tst:
                x = x + 1
            x = {op}x
            return box(x)
        """
        with self.subTest(type=type, op=op, x=x, res=res):
            f = self.run_code(codestr)["testfunc"]
            self.assertEqual(f(False), res, f"{type} {op} {x} {res}")
