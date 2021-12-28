# Reason: Test hitted a banned word mixed_args
def test_verify_mixed_args_positional_failure_method(self):
    codestr = """
        class C:
            def x(self, a: int=1, b: str="hunter2", c: int=14) -> None:
                return
        C().x("hi", b="lol")
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"Exact\[str\] received for positional arg 'a', expected int",
    ):
        self.compile(codestr)
