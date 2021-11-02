def test_verify_too_many_args(self):
    codestr = """
        def x():
            return 42
        x(1)
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"Mismatched number of args for function <module>.x. Expected 0, got 1",
    ):
        self.compile(codestr)
