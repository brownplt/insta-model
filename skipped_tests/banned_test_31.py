# Reason: Test hitted a banned word int64
def test_int_large_int_constant(self):
    codestr = """
        from __static__ import int64
        def testfunc(tst):
            x: int64 = 0x7FFFFFFF + 1
    """
    code = self.compile(codestr)
    f = self.find_code(code)
    self.assertInBytecode(f, "PRIMITIVE_LOAD_CONST", (0x80000000, TYPED_INT64))
