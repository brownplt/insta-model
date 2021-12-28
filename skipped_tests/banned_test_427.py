# Reason: Test hitted a banned word float
def test_double_box(self):
    codestr = """
    from __static__ import double, box
    def t() -> float:
        pi: double = 3.14159
        return box(pi)
    """
    with self.in_module(codestr) as mod:
        t = mod.t
        self.assertInBytecode(t, "PRIMITIVE_LOAD_CONST", (3.14159, TYPED_DOUBLE))
        self.assertNotInBytecode(t, "CAST")
        self.assertEqual(t(), 3.14159)
        self.assert_jitted(t)
