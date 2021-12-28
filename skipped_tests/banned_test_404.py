# Reason: Test hitted a banned word int8
def test_array_set_success_dynamic_subscript_2(self):
    codestr = """
        from __static__ import Array, int8
        def x():
            return 1
        def m() -> Array[int8]:
            a = Array[int8]([1, 3, -5])
            v: int8 = 37
            a[x()] = v
            return a
    """
    c = self.compile(codestr, modname="foo.py")
    m = self.find_code(c, "m")
    self.assertInBytecode(m, "PRIMITIVE_LOAD_CONST", (37, TYPED_INT8))
    with self.in_module(codestr) as mod:
        m = mod.m
        r = m()
        self.assertEqual(r, array("b", [1, 37, -5]))
