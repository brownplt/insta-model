# Reason: Test hitted a banned word double
def test_double_load_const(self):
    codestr = """
    from __static__ import double
    def t():
        pi: double = 3.14159
    """
    with self.in_module(codestr) as mod:
        t = mod.t
        self.assertInBytecode(t, "PRIMITIVE_LOAD_CONST", (3.14159, TYPED_DOUBLE))
        t()
        self.assert_jitted(t)
