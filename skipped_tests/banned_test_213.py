# Reason: Test hitted a banned word box
def test_tuple_assign_constant(self):
    codestr = """
        from __static__ import int16, box
        def testfunc():
            x: int
            y: str
            x, y = 1, 1
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"type mismatch: Exact\[int\] cannot be assigned to str",
    ):
        self.compile(codestr, modname="foo")
