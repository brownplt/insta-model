# Reason: Test hitted a banned word int8
def test_int_assign_str_constant(self):
    codestr = """
        from __static__ import int8
        def testfunc(tst):
            x: int8 = 'abc' + 'def'
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, type_mismatch("Exact[str]", "int8")
    ):
        self.compile(codestr)
