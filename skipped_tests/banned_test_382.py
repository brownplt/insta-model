# Reason: Test hitted a banned word int8
def test_load_int_const_signed(self):
    int_types = [
        "int8",
        "int16",
        "int32",
        "int64",
    ]
    signs = ["-", ""]
    values = [12]
    for type, sign, value in itertools.product(int_types, signs, values):
        expected = value if sign == "" else -value
        codestr = f"""
            from __static__ import {type}, box
            def y() -> int:
                x: {type} = {sign}{value}
                return box(x)
        """
        with self.subTest(type=type, sign=sign, value=value):
            y = self.find_code(self.compile(codestr, modname="foo"), name="y")
            self.assertInBytecode(y, "PRIMITIVE_LOAD_CONST")
            with self.in_module(codestr) as mod:
                y = mod.y
                self.assertEqual(y(), expected)
