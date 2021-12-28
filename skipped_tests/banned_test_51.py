# Reason: Test hitted a banned word int8
def test_primitive_arithmetic(self):
    cases = [
        ("int8", 127, "*", 1, 127),
        ("int8", -64, "*", 2, -128),
        ("int8", 0, "*", 4, 0),
        ("uint8", 51, "*", 5, 255),
        ("uint8", 5, "*", 0, 0),
        ("int16", 3123, "*", -10, -31230),
        ("int16", -32767, "*", -1, 32767),
        ("int16", -32768, "*", 1, -32768),
        ("int16", 3, "*", 0, 0),
        ("uint16", 65535, "*", 1, 65535),
        ("uint16", 0, "*", 4, 0),
        ("int32", (1 << 31) - 1, "*", 1, (1 << 31) - 1),
        ("int32", -(1 << 30), "*", 2, -(1 << 31)),
        ("int32", 0, "*", 1, 0),
        ("uint32", (1 << 32) - 1, "*", 1, (1 << 32) - 1),
        ("uint32", 0, "*", 4, 0),
        ("int64", (1 << 63) - 1, "*", 1, (1 << 63) - 1),
        ("int64", -(1 << 62), "*", 2, -(1 << 63)),
        ("int64", 0, "*", 1, 0),
        ("uint64", (1 << 64) - 1, "*", 1, (1 << 64) - 1),
        ("uint64", 0, "*", 4, 0),
        ("int8", 127, "//", 4, 31),
        ("int8", -128, "//", 4, -32),
        ("int8", 0, "//", 4, 0),
        ("uint8", 255, "//", 5, 51),
        ("uint8", 0, "//", 5, 0),
        ("int16", 32767, "//", -1000, -32),
        ("int16", -32768, "//", -1000, 32),
        ("int16", 0, "//", 4, 0),
        ("uint16", 65535, "//", 5, 13107),
        ("uint16", 0, "//", 4, 0),
        ("int32", (1 << 31) - 1, "//", (1 << 31) - 1, 1),
        ("int32", -(1 << 31), "//", 1, -(1 << 31)),
        ("int32", 0, "//", 1, 0),
        ("uint32", (1 << 32) - 1, "//", 500, 8589934),
        ("uint32", 0, "//", 4, 0),
        ("int64", (1 << 63) - 1, "//", 2, (1 << 62) - 1),
        ("int64", -(1 << 63), "//", 2, -(1 << 62)),
        ("int64", 0, "//", 1, 0),
        ("uint64", (1 << 64) - 1, "//", (1 << 64) - 1, 1),
        ("uint64", 0, "//", 4, 0),
        ("int8", 127, "%", 4, 3),
        ("int8", -128, "%", 4, 0),
        ("int8", 0, "%", 4, 0),
        ("uint8", 255, "%", 6, 3),
        ("uint8", 0, "%", 5, 0),
        ("int16", 32767, "%", -1000, 767),
        ("int16", -32768, "%", -1000, -768),
        ("int16", 0, "%", 4, 0),
        ("uint16", 65535, "%", 7, 1),
        ("uint16", 0, "%", 4, 0),
        ("int32", (1 << 31) - 1, "%", (1 << 31) - 1, 0),
        ("int32", -(1 << 31), "%", 1, 0),
        ("int32", 0, "%", 1, 0),
        ("uint32", (1 << 32) - 1, "%", 500, 295),
        ("uint32", 0, "%", 4, 0),
        ("int64", (1 << 63) - 1, "%", 2, 1),
        ("int64", -(1 << 63), "%", 2, 0),
        ("int64", 0, "%", 1, 0),
        ("uint64", (1 << 64) - 1, "%", (1 << 64) - 1, 0),
        ("uint64", 0, "%", 4, 0),
    ]
    for typ, a, op, b, res in cases:
        for const in ["noconst", "constfirst", "constsecond"]:
            if const == "noconst":
                codestr = f"""
                    from __static__ import {typ}
                    def f(a: {typ}, b: {typ}) -> {typ}:
                        return a {op} b
                """
            elif const == "constfirst":
                codestr = f"""
                    from __static__ import {typ}
                    def f(b: {typ}) -> {typ}:
                        return {a} {op} b
                """
            elif const == "constsecond":
                codestr = f"""
                    from __static__ import {typ}
                    def f(a: {typ}) -> {typ}:
                        return a {op} {b}
                """
            with self.subTest(typ=typ, a=a, op=op, b=b, res=res, const=const):
                with self.in_module(codestr) as mod:
                    f = mod.f
                    act = None
                    if const == "noconst":
                        act = f(a, b)
                    elif const == "constfirst":
                        act = f(b)
                    elif const == "constsecond":
                        act = f(a)
                    self.assertEqual(act, res)
