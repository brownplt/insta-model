# Reason: Test hitted some skipped words
def test_final_reassigned_in_except(self):
    codestr = """
    from typing import Final
    def f():
        e: Final[int] = 3
        try:
            x = 1 + "2"
        except Exception as e:
            pass
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Cannot assign to a Final variable"
    ):
        self.compile(codestr, modname="foo")
