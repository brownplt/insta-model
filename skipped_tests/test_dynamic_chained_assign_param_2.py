# Reason: Test hitted some skipped words
def test_dynamic_chained_assign_param_2(self):
    codestr = """
        from __static__ import int16
        def testfunc(y):
            x: int16
            y = x = 42
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, type_mismatch("Literal[42]", "dynamic")
    ):
        self.compile(codestr, modname="foo")
