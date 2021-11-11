# Reason: Test hitted some skipped words
def test_unannotated_assign_does_not_declare_type(self) -> None:
    codestr = """
        def f(flag):
            x = None
            if flag:
                x = "foo"
            reveal_type(x)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"'x' has declared type 'dynamic' and local type 'Optional\[str\]'",
    ):
        self.compile(codestr)
