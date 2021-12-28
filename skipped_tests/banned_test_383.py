# Reason: Test hitted a banned word int8
def test_load_int_const_unsigned(self):
    int_types = [
        "uint8",
        "uint16",
        "uint32",
        "uint64",
    ]
    values = [12]
    for type, value in itertools.product(int_types, values):
        expected = value
        codestr = f"""
            from __static__ import {type}, box
            def y() -> int:
                return box({type}({value}))
        """
        with self.subTest(type=type, value=value):
            y = self.find_code(self.compile(codestr, modname="foo"), name="y")
            self.assertInBytecode(y, "PRIMITIVE_LOAD_CONST")
            with self.in_module(codestr) as mod:
                y = mod.y
                self.assertEqual(y(), expected)
