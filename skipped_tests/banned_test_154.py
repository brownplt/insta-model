# Reason: Test hitted a banned word _kw
def test_incompat_override_method_kwonly_name(self):
    codestr = """
        class A:
            def m(self, *, y: int) -> int:
                return 42
        class B(A):
            def m(self, *, x: int) -> int:
                return 0
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        "<module>.B.m overrides <module>.A.m inconsistently. Keyword only argument `y` is overridden as `x`",
    ):
        self.compile(codestr)
