# Reason: Test hitted a banned word int8
def test_array_set_negative_idx(self):
    codestr = """
        from __static__ import Array, int8
        def m() -> Array[int8]:
            a = Array[int8]([1, 3, -5])
            a[-2] = 7
            return a
    """
    c = self.compile(codestr, modname="foo.py")
    m = self.find_code(c, "m")
    self.assertInBytecode(m, "PRIMITIVE_LOAD_CONST", (7, TYPED_INT8))
    self.assertInBytecode(m, "LOAD_CONST", -2)
    self.assertInBytecode(m, "SEQUENCE_SET", SEQ_ARRAY_INT8)
    with self.in_module(codestr) as mod:
        m = mod.m
        self.assertEqual(m(), array("h", [1, 7, -5]))
