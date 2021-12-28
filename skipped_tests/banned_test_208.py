# Reason: Test hitted a banned word box
def test_dynamic_chained_assign_param(self):
    codestr = """
        from __static__ import int16, box
        def testfunc(y):
            x: int16
            x = y = 42
            return box(x)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, type_mismatch("Literal[42]", "int16")
    ):
        self.compile(codestr, modname="foo")
