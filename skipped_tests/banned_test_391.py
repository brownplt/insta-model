# Reason: Test hitted a banned word int8
def test_array_get_primitive_idx(self):
    codestr = """
        from __static__ import Array, int8, box
        def m() -> int:
            content = list(range(121))
            a = Array[int8](content)
            return box(a[int8(111)])
    """
    c = self.compile(codestr, modname="foo.py")
    m = self.find_code(c, "m")
    self.assertInBytecode(m, "PRIMITIVE_LOAD_CONST", (111, TYPED_INT8))
    self.assertInBytecode(m, "SEQUENCE_GET", SEQ_ARRAY_INT8)
    with self.in_module(codestr) as mod:
        m = mod.m
        actual = m()
        self.assertEqual(actual, 111)
