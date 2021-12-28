# Reason: Test hitted a banned word int64
def test_chained_assign_type_inference(self):
    codestr = """
        from __static__ import int64, char, Array
        def test2():
            y = x = 4
            reveal_type(x)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"'x' has declared type 'dynamic' and local type 'Literal\[4\]'",
    ):
        self.compile(codestr, modname="foo")
