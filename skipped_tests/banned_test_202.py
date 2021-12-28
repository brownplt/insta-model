# Reason: Test hitted a banned word int8
def test_narrowing_assign_out_of_range(self):
    codestr = """
        from __static__ import int8, int16, box
        def testfunc():
            x: int8
            y: int16
            x = y = 300
            return box(x), box(y)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        "type mismatch: Literal\\[300\\] cannot be assigned to int8",
    ):
        self.compile(codestr, modname="foo")
