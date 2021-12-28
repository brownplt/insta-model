# Reason: Test hitted a banned word int64
def test_int_bad_assign(self):
    with self.assertRaisesRegex(
        TypedSyntaxError, type_mismatch("Exact[str]", "int64")
    ):
        code = self.compile(
            """
        from __static__ import ssize_t
        def f():
            x: ssize_t = 'abc'
        """,
            StaticCodeGenerator,
        )
