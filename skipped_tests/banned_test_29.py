# Reason: Test hitted a banned word float
def test_int_assign_float(self):
    codestr = """
        from __static__ import int8
        def testfunc(tst):
            x: int8 = 1.0
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, type_mismatch("Exact[float]", "int")
    ):
        self.compile(codestr)
