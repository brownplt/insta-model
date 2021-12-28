# Reason: Test hitted a banned word int8
def test_load_int_const_sizes(self):
    cases = [
        ("int8", (1 << 7), True),
        ("int8", (-1 << 7) - 1, True),
        ("int8", -1 << 7, False),
        ("int8", (1 << 7) - 1, False),
        ("int16", (1 << 15), True),
        ("int16", (-1 << 15) - 1, True),
        ("int16", -1 << 15, False),
        ("int16", (1 << 15) - 1, False),
        ("int32", (1 << 31), True),
        ("int32", (-1 << 31) - 1, True),
        ("int32", -1 << 31, False),
        ("int32", (1 << 31) - 1, False),
        ("int64", (1 << 63), True),
        ("int64", (-1 << 63) - 1, True),
        ("int64", -1 << 63, False),
        ("int64", (1 << 63) - 1, False),
        ("uint8", (1 << 8), True),
        ("uint8", -1, True),
        ("uint8", (1 << 8) - 1, False),
        ("uint8", 0, False),
        ("uint16", (1 << 16), True),
        ("uint16", -1, True),
        ("uint16", (1 << 16) - 1, False),
        ("uint16", 0, False),
        ("uint32", (1 << 32), True),
        ("uint32", -1, True),
        ("uint32", (1 << 32) - 1, False),
        ("uint32", 0, False),
        ("uint64", (1 << 64), True),
        ("uint64", -1, True),
        ("uint64", (1 << 64) - 1, False),
        ("uint64", 0, False),
    ]
    for type, val, overflows in cases:
        codestr = f"""
            from __static__ import {type}, box
            def f() -> int:
                x: {type} = {val}
                return box(x)
        """
        with self.subTest(type=type, val=val, overflows=overflows):
            if overflows:
                with self.assertRaisesRegex(
                    TypedSyntaxError,
                    f"type mismatch: Literal\\[{val}\\] cannot be assigned to {type}",
                ):
                    self.compile(codestr)
            else:
                with self.in_strict_module(codestr) as mod:
                    self.assertEqual(mod.f(), val)
