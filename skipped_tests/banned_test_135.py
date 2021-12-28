# Reason: Test hitted a banned word _kw
def test_incompat_override_method_kwonly_mismatch(self):
    codestr = """
        class A:
            def m(self, x: str) -> int:
                return 42
        class B(A):
            def m(self, *, x: str) -> int:
                return 0
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        "<module>.B.m overrides <module>.A.m inconsistently. `x` differs by keyword only vs positional",
    ):
        self.compile(codestr)
