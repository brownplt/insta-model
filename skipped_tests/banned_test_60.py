# Reason: Test hitted a banned word int64
def test_int_loop(self):
    codestr = """
    from __static__ import ssize_t, box
    def testfunc():
        i: ssize_t = 0
        while i < 100:
            i = i + 1
        return box(i)
    """
    code = self.compile(codestr)
    f = self.find_code(code)
    f = self.run_code(codestr)["testfunc"]
    self.assertEqual(f(), 100)
    self.assertInBytecode(f, "PRIMITIVE_LOAD_CONST", (0, TYPED_INT64))
    self.assertInBytecode(f, "LOAD_LOCAL", (0, ("__static__", "int64")))
    self.assertInBytecode(f, "PRIMITIVE_BINARY_OP", PRIM_OP_ADD_INT)
    self.assertInBytecode(f, "PRIMITIVE_COMPARE_OP", PRIM_OP_LT_INT)
    self.assertInBytecode(f, "POP_JUMP_IF_ZERO")
