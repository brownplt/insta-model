# Reason: Test hitted a banned word cbool
def test_cbool_cast(self):
    codestr = """
    from __static__ import cbool
    def f(y: bool) -> int:
        x: cbool = y
        if x:
            return 1
        else:
            return 2
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, type_mismatch("Exact[bool]", "cbool")
    ):
        self.compile(codestr, modname="foo")
