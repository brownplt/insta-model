# Reason: Test hitted a banned word box
def test_bad_box(self):
    codestr = """
    from __static__ import box
    box('abc')
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "can't box non-primitive: Exact\\[str\\]"
    ):
        self.compile(codestr)
