# Reason: Test hitted a banned word int8
def test_int_unary(self):
    tests = [
        ("int8", "-", 1, -1),
        ("uint8", "-", 1, (1 << 8) - 1),
        ("int16", "-", 1, -1),
        ("int16", "-", 256, -256),
        ("uint16", "-", 1, (1 << 16) - 1),
        ("int32", "-", 1, -1),
        ("int32", "-", 65536, -65536),
        ("uint32", "-", 1, (1 << 32) - 1),
        ("int64", "-", 1, -1),
        ("int64", "-", 1 << 32, -(1 << 32)),
        ("uint64", "-", 1, (1 << 64) - 1),
        ("int8", "~", 1, -2),
        ("uint8", "~", 1, (1 << 8) - 2),
        ("int16", "~", 1, -2),
        ("uint16", "~", 1, (1 << 16) - 2),
        ("int32", "~", 1, -2),
        ("uint32", "~", 1, (1 << 32) - 2),
        ("int64", "~", 1, -2),
        ("uint64", "~", 1, (1 << 64) - 2),
    ]
    for type, op, x, res in tests:
        codestr = f"""
        from __static__ import {type}, box
        def testfunc(tst):
            x: {type} = {x}
            if tst:
                x = x + 1
            x = {op}x
            return box(x)
        """
        with self.subTest(type=type, op=op, x=x, res=res):
            code = self.compile(codestr)
            f = self.run_code(codestr)["testfunc"]
            self.assertEqual(f(False), res, f"{type} {op} {x} {res}")
