# Reason: Test hitted a banned word mixed_args
def test_verify_mixed_args_positional_failure(self):
    codestr = """
        def x(a: int=1, b: str="hunter2", c: int=14) -> None:
            return
        x("hi", b="lol")
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"Exact\[str\] received for positional arg 'a', expected int",
    ):
        self.compile(codestr)
# Same tests as above, but for methods.
