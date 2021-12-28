# Reason: Test hitted a banned word int8
def test_primitive_args_and_return(self):
    cases = [
        ("cbool", 1),
        ("cbool", 0),
        ("int8", -1 << 7),
        ("int8", (1 << 7) - 1),
        ("int16", -1 << 15),
        ("int16", (1 << 15) - 1),
        ("int32", -1 << 31),
        ("int32", (1 << 31) - 1),
        ("int64", -1 << 63),
        ("int64", (1 << 63) - 1),
        ("uint8", (1 << 8) - 1),
        ("uint8", 0),
        ("uint16", (1 << 16) - 1),
        ("uint16", 0),
        ("uint32", (1 << 32) - 1),
        ("uint32", 0),
        ("uint64", (1 << 64) - 1),
        ("uint64", 0),
    ]
    for typ, val in cases:
        if typ == "cbool":
            op = "or"
            expected = True
            other = "cbool(True)"
            boxed = "bool"
        else:
            op = "+" if val <= 0 else "-"
            expected = val + (1 if op == "+" else -1)
            other = "1"
            boxed = "int"
        with self.subTest(typ=typ, val=val, op=op, expected=expected):
            codestr = f"""
                from __static__ import {typ}, box
                def f(x: {typ}, y: {typ}) -> {typ}:
                    return x {op} y
                def g() -> {boxed}:
                    return box(f({val}, {other}))
            """
            with self.in_strict_module(codestr) as mod:
                self.assertEqual(mod.g(), expected)
