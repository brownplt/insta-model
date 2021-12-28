# Reason: Test hitted a banned word _kw
def test_verify_mixed_args_kw_failure(self):
    codestr = """
        def x(a: int=1, b: str="hunter2", c: int=14) -> None:
            return
        x(12, c="hi", b="lol")
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, r"Exact\[str\] received for keyword arg 'c', expected int"
    ):
        self.compile(codestr)
