# Reason: Test hitted a banned word int8
def test_widening_assign_reassign_error(self):
    codestr = """
        from __static__ import int8, int16, box
        def testfunc():
            x: int16
            y: int8
            x = y = 42
            y = 128
            return box(x), box(y)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        "type mismatch: Literal\\[128\\] cannot be assigned to int8",
    ):
        self.compile(codestr, modname="foo")
