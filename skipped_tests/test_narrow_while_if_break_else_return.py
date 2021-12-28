# Reason: Code hitted some skipped words
def test_narrow_while_if_break_else_return(self):
    codestr = """
        from typing import Optional
        def f(x: Optional[int], y: int) -> int:
            while x is None:
                if y > 0:
                    break
                else:
                    return 42
            return x
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        bad_ret_type("Optional[int]", "int"),
    ):
        self.compile(codestr)
