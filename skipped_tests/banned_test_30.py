# Reason: Test hitted a banned word int8
def test_int_constant_range(self):
    for type, val, low, high in [
        ("int8", 128, -128, 127),
        ("int8", -129, -128, 127),
        ("int16", 32768, -32768, 32767),
        ("int16", -32769, -32768, 32767),
        ("int32", 2147483648, -2147483648, 2147483647),
        ("int32", -2147483649, -2147483648, 2147483647),
        ("int64", 9223372036854775808, -9223372036854775808, 9223372036854775807),
        ("int64", -9223372036854775809, -9223372036854775808, 9223372036854775807),
        ("uint8", 257, 0, 255),
        ("uint8", -1, 0, 255),
        ("uint16", 65537, 0, 65535),
        ("uint16", -1, 0, 65535),
        ("uint32", 4294967297, 0, 4294967295),
        ("uint32", -1, 0, 4294967295),
        ("uint64", 18446744073709551617, 0, 18446744073709551615),
        ("uint64", -1, 0, 18446744073709551615),
    ]:
        codestr = f"""
            from __static__ import {type}
            def testfunc(tst):
                x: {type} = {val}
        """
        with self.subTest(type=type, val=val, low=low, high=high):
            with self.assertRaisesRegex(
                TypedSyntaxError,
                f"type mismatch: Literal\\[{val}\\] cannot be assigned to {type}",
            ):
                self.compile(codestr)
