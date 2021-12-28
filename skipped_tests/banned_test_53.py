# Reason: Test hitted a banned word int32
def test_int_compare_mixed_sign(self):
    tests = [
        ("uint16", 10000, "int16", -1, "<", False),
        ("uint16", 10000, "int16", -1, "<=", False),
        ("int16", -1, "uint16", 10000, ">", False),
        ("int16", -1, "uint16", 10000, ">=", False),
        ("uint32", 10000, "int16", -1, "<", False),
    ]
    for type1, x, type2, y, op, res in tests:
        codestr = f"""
        from __static__ import {type1}, {type2}, box
        def testfunc(tst):
            x: {type1} = {x}
            y: {type2} = {y}
            if tst:
                x = x + 1
                y = y + 2
            if x {op} y:
                return True
            return False
        """
        with self.subTest(type1=type1, x=x, type2=type2, y=y, op=op, res=res):
            code = self.compile(codestr)
            f = self.run_code(codestr)["testfunc"]
            self.assertEqual(f(False), res, f"{type} {x} {op} {y} {res}")
