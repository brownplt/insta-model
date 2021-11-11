# Reason: Test hitted some skipped words
def test_static_function_incompat_override(self):
    codestr = """
        class A:
            @staticmethod
            def m() -> int:
                return 42
        class B(A):
            @staticmethod
            def m() -> str:
                return 0
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        "<module>.B.m overrides <module>.A.m inconsistently. "
        "Returned type `str` is not a subtype of the overridden return `int`",
    ):
        self.compile(codestr)
