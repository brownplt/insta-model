def test_narrow_while_break(self):
    codestr = """
        from typing import Optional
        def f(x: Optional[int]) -> int:
            while x is None:
                break
            return x
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        bad_ret_type("Optional[int]", "int"),
    ):
        self.compile(codestr)
