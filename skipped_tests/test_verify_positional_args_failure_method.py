# Reason: Test hitted some skipped words
def test_verify_positional_args_failure_method(self):
    codestr = """
        class C:
            def x(self, a: int, b: str) -> None:
                pass
        C().x("a", 2)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"Exact\[str\] received for positional arg 'a', expected int",
    ):
        self.compile(codestr)
