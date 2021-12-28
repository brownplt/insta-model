# Reason: Test hitted a banned word int8
def test_array_set_failure_invalid_subscript(self):
    codestr = """
        from __static__ import Array, int8
        def x():
            return object()
        def m() -> Array[int8]:
            a = Array[int8]([1, 3, -5])
            a[x()] = 7
            return a
    """
    c = self.compile(codestr, modname="foo.py")
    m = self.find_code(c, "m")
    self.assertInBytecode(m, "PRIMITIVE_LOAD_CONST", (7, TYPED_INT8))
    with self.in_module(codestr) as mod:
        m = mod.m
        with self.assertRaisesRegex(TypeError, "array indices must be integers"):
            m()
