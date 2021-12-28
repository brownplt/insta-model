# Reason: Test hitted a banned word int8
def test_array_get_nonprimitive_idx(self):
    codestr = """
        from __static__ import Array, int8, box
        def m() -> int:
            content = list(range(121))
            a = Array[int8](content)
            return box(a[111])
    """
    c = self.compile(codestr, modname="foo.py")
    m = self.find_code(c, "m")
    self.assertInBytecode(m, "LOAD_CONST", 111)
    self.assertNotInBytecode(m, "PRIMITIVE_LOAD_CONST")
    self.assertInBytecode(m, "PRIMITIVE_UNBOX")
    self.assertInBytecode(m, "SEQUENCE_GET", SEQ_ARRAY_INT8)
    with self.in_module(codestr) as mod:
        m = mod.m
        actual = m()
        self.assertEqual(actual, 111)
