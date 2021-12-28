# Reason: Test hitted a banned word int64
def test_unbox_kw_args(self):
    code = """
        from __static__ import int64, unbox
        def f(a):
            x: int64 = unbox(42, x=2)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "unbox\\(\\) takes no keyword arguments"
    ):
        self.compile(code)
