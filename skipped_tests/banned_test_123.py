# Reason: Test hitted a banned word int64
def test_len_kw_args(self):
    code = """
        from __static__ import int64
        def f(a):
            len([], x=2)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "len\\(\\) takes no keyword arguments"
    ):
        self.compile(code)
