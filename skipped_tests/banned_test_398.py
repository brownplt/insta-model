# Reason: Test hitted a banned word int8
def test_array_set_signed(self):
    int_types = [
        "int8",
        "int16",
        "int32",
        "int64",
    ]
    seq_types = {
        "int8": SEQ_ARRAY_INT8,
        "int16": SEQ_ARRAY_INT16,
        "int32": SEQ_ARRAY_INT32,
        "int64": SEQ_ARRAY_INT64,
    }
    signs = ["-", ""]
    value = 77
    for type, sign in itertools.product(int_types, signs):
        codestr = f"""
            from __static__ import Array, {type}
            def m() -> Array[{type}]:
                a = Array[{type}]([1, 3, -5])
                a[1] = {sign}{value}
                return a
        """
        with self.subTest(type=type, sign=sign):
            val = -value if sign else value
            c = self.compile(codestr, modname="foo.py")
            m = self.find_code(c, "m")
            self.assertInBytecode(
                m, "PRIMITIVE_LOAD_CONST", (val, prim_name_to_type[type])
            )
            self.assertInBytecode(m, "LOAD_CONST", 1)
            self.assertInBytecode(m, "SEQUENCE_SET", seq_types[type])
            with self.in_module(codestr) as mod:
                m = mod.m
                if sign:
                    expected = -value
                else:
                    expected = value
                result = m()
                self.assertEqual(
                    result,
                    array("q", [1, expected, -5]),
                    f"Failing case: {type}, {sign}",
                )
