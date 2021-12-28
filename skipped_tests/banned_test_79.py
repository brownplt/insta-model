# Reason: Test hitted a banned word box
def test_bad_box_2(self):
    codestr = """
    from __static__ import box
    box('abc', 'foo')
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "box only accepts a single argument"
    ):
        self.compile(codestr)
