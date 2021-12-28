# Reason: Test hitted a banned word int8
def test_array_set_failure(self) -> object:
    codestr = """
        from __static__ import Array, int8
        def m() -> Array[int8]:
            a = Array[int8]([1, 3, -5])
            a[-100] = 7
            return a
    """
    c = self.compile(codestr, modname="foo.py")
    m = self.find_code(c, "m")
    self.assertInBytecode(m, "PRIMITIVE_LOAD_CONST", (7, TYPED_INT8))
    self.assertInBytecode(m, "SEQUENCE_SET", SEQ_ARRAY_INT8)
    with self.in_module(codestr) as mod:
        m = mod.m
        with self.assertRaisesRegex(IndexError, "index out of range"):
            m()
