# Reason: Test hitted a banned word float
def test_double_binop(self):
    tests = [
        (1.732, 2.0, "+", 3.732),
        (1.732, 2.0, "-", -0.268),
        (1.732, 2.0, "/", 0.866),
        (1.732, 2.0, "*", 3.464),
        (1.732, 2, "+", 3.732),
    ]
    if cinderjit is not None:
        # test for division by zero
        tests.append((1.732, 0.0, "/", float("inf")))
    for x, y, op, res in tests:
        codestr = f"""
        from __static__ import double, box
        def testfunc(tst):
            x: double = {x}
            y: double = {y}
            z: double = x {op} y
            return box(z)
        """
        with self.subTest(type=type, x=x, y=y, op=op, res=res):
            with self.in_module(codestr) as mod:
                f = mod.testfunc
                self.assertEqual(f(False), res, f"{type} {x} {op} {y} {res}")
