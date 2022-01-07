# Reason: Hitted a skipped word (@staticmethod)
def test_static_function_incompat_override_arg(self):
    codestr = """
        class A:
            @staticmethod
            def m(a: int) -> int:
                return 42
        class B(A):
            @staticmethod
            def m(a: str) -> int:
                return 0
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        "<module>.B.m overrides <module>.A.m inconsistently. "
        "Parameter a of type `str` is not a subtype of the overridden parameter `int`",
    ):
        self.compile(codestr)
