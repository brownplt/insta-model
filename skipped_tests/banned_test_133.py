# Reason: Test hitted a banned word _kw
def test_incompat_override_method_num_kwonly_args(self):
    codestr = """
        class A:
            def m(self) -> int:
                return 42
        class B(A):
            def m(self, *, x: int) -> int:
                return 0
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        "<module>.B.m overrides <module>.A.m inconsistently. Number of arguments differ",
    ):
        self.compile(codestr)
