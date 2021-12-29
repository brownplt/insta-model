# Reason: Hitted a skipped word (reveal_type)
def test_reveal_type_local(self) -> None:
    codestr = """
        def f(x: int | None):
            if x is not None:
                reveal_type(x)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"reveal_type\(x\): 'int', 'x' has declared type 'Optional\[int\]' and local type 'int'",
    ):
        self.compile(codestr)
