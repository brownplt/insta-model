# Reason: Test hitted a banned word double
def test_double_compare(self):
    tests = [
        (1.0, 2.0, "==", False),
        (1.0, 2.0, "!=", True),
        (1.0, 2.0, "<", True),
        (1.0, 2.0, "<=", True),
        (2.0, 1.0, "<", False),
        (2.0, 1.0, "<=", False),
    ]
    for x, y, op, res in tests:
        codestr = f"""
        from __static__ import double, box
        def testfunc(tst):
            x: double = {x}
            y: double = {y}
            if tst:
                x = x + 1
                y = y + 2
            if x {op} y:
                return True
            return False
        """
        with self.subTest(type=type, x=x, y=y, op=op, res=res):
            f = self.run_code(codestr)["testfunc"]
            self.assertEqual(f(False), res, f"{type} {x} {op} {y} {res}")
