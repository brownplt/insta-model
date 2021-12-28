# Reason: Test hitted a banned word int8
def test_array_set_unsigned(self):
    uint_types = [
        "uint8",
        "uint16",
        "uint32",
        "uint64",
    ]
    value = 77
    seq_types = {
        "uint8": SEQ_ARRAY_UINT8,
        "uint16": SEQ_ARRAY_UINT16,
        "uint32": SEQ_ARRAY_UINT32,
        "uint64": SEQ_ARRAY_UINT64,
    }
    for type in uint_types:
        codestr = f"""
            from __static__ import Array, {type}
            def m() -> Array[{type}]:
                a = Array[{type}]([1, 3, 5])
                a[1] = {value}
                return a
        """
        with self.subTest(type=type):
            c = self.compile(codestr, modname="foo.py")
            m = self.find_code(c, "m")
            self.assertInBytecode(
                m, "PRIMITIVE_LOAD_CONST", (value, prim_name_to_type[type])
            )
            self.assertInBytecode(m, "LOAD_CONST", 1)
            self.assertInBytecode(m, "SEQUENCE_SET", seq_types[type])
            with self.in_module(codestr) as mod:
                m = mod.m
                expected = value
                result = m()
                self.assertEqual(
                    result, array("q", [1, expected, 5]), f"Failing case: {type}"
                )
