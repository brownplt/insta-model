# Reason: Test hitted a banned word int8
def test_unbox(self):
    for size, val in [
        ("int8", 126),
        ("int8", -128),
        ("int16", 32766),
        ("int16", -32768),
        ("int32", 2147483646),
        ("int32", -2147483648),
        ("int64", 9223372036854775806),
        ("int64", -9223372036854775808),
        ("uint8", 254),
        ("uint16", 65534),
        ("uint32", 4294967294),
        ("uint64", 18446744073709551614),
    ]:
        codestr = f"""
        from __static__ import {size}, box, unbox
        def f(x):
            y: {size} = unbox(x)
            y = y + 1
            return box(y)
        """
        code = self.compile(codestr)
        f = self.find_code(code)
        f = self.run_code(codestr)["f"]
        self.assertEqual(f(val), val + 1)
