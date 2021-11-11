# Reason: Test hitted some skipped words
def test_assign_chained_failure_wrong_target_type(self):
    codestr = """
        def test() -> str:
            x: int = 1
            y = x = "hello"
            return y
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, type_mismatch("Exact[str]", "int")
    ):
        self.compile(codestr, modname="foo")
