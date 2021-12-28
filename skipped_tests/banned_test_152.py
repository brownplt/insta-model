# Reason: Test hitted a banned word *args
def test_incompat_override_method_starargs(self):
    codestr = """
        class A:
            def m(self) -> int:
                return 42
        class B(A):
            def m(self, *args) -> int:
                return 0
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        "<module>.B.m overrides <module>.A.m inconsistently. "
        "Functions differ by including \\*args",
    ):
        self.compile(codestr)
