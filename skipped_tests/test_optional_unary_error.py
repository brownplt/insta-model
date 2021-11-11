# Reason: Test hitted some skipped words
def test_optional_unary_error(self) -> None:
    codestr = """
        from typing import Optional
        def f(a: Optional[int]):
            -a
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        re.escape("Optional[int]: bad operand type for unary -: 'NoneType'"),
    ):
        self.compile(codestr)
