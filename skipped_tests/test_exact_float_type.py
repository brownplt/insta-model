# Reason: Test hitted some skipped words
def test_exact_float_type(self):
    codestr = """
    def foo():
        f = float("1.0")
        reveal_type(f)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"reveal_type\(f\): 'Exact\[float\]'",
    ):
        self.compile(codestr)
