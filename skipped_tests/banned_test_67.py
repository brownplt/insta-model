# Reason: Test hitted a banned word int64
def test_compat_int_math(self):
    codestr = """
    from __static__ import ssize_t, box
    def f():
        x: ssize_t = 42
        z: ssize_t = 1 + x
        return box(z)
    """
    code = self.compile(codestr)
    f = self.find_code(code)
    self.assertInBytecode(f, "PRIMITIVE_LOAD_CONST", (42, TYPED_INT64))
    self.assertInBytecode(f, "LOAD_LOCAL", (0, ("__static__", "int64")))
    self.assertInBytecode(f, "PRIMITIVE_BINARY_OP", PRIM_OP_ADD_INT)
    f = self.run_code(codestr)["f"]
    self.assertEqual(f(), 43)
