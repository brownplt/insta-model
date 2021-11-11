# Reason: Test hitted some skipped words
def test_reveal_type(self) -> None:
    codestr = """
        def f(x: int):
            reveal_type(x or None)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"reveal_type\(x or None\): 'Optional\[int\]'",
    ):
        self.compile(codestr)
