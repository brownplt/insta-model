# Reason: Test hitted a banned word int8
def test_unbox_overflow(self):
    cases = [
        (
            "uint8",
            [
                (-1, OverflowError),
                (0, 0),
                (255, 255),
                (256, OverflowError),
            ],
        ),
        (
            "int8",
            [
                (-129, OverflowError),
                (-128, -128),
                (127, 127),
                (128, OverflowError),
            ],
        ),
        (
            "uint16",
            [
                (-1, OverflowError),
                (0, 0),
                ((1 << 16) - 1, (1 << 16) - 1),
                ((1 << 16), OverflowError),
            ],
        ),
        (
            "int16",
            [
                (-(1 << 15) - 1, OverflowError),
                (-(1 << 15), -(1 << 15)),
                ((1 << 15) - 1, (1 << 15) - 1),
                ((1 << 15), OverflowError),
            ],
        ),
        (
            "uint32",
            [
                (-1, OverflowError),
                (0, 0),
                ((1 << 32) - 1, (1 << 32) - 1),
                ((1 << 32), OverflowError),
            ],
        ),
        (
            "int32",
            [
                (-(1 << 31) - 1, OverflowError),
                (-(1 << 31), -(1 << 31)),
                ((1 << 31) - 1, (1 << 31) - 1),
                ((1 << 31), OverflowError),
            ],
        ),
        (
            "uint64",
            [
                (-1, OverflowError),
                (0, 0),
                ((1 << 64) - 1, (1 << 64) - 1),
                ((1 << 64), OverflowError),
            ],
        ),
        (
            "int64",
            [
                (-(1 << 63) - 1, OverflowError),
                (-(1 << 63), -(1 << 63)),
                ((1 << 63) - 1, (1 << 63) - 1),
                ((1 << 63), OverflowError),
            ],
        ),
    ]
    for typ, values in cases:
        codestr = f"""
            from __static__ import {typ}
            def f(x: int) -> {typ}:
                return {typ}(x)
        """
        with self.in_module(codestr) as mod:
            f = mod.f
            for val, result in values:
                with self.subTest(typ=typ, val=val, result=result):
                    if result is OverflowError:
                        with self.assertRaises(result):
                            f(val)
                    else:
                        self.assertEqual(f(val), val)
