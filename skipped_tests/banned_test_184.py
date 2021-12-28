# Reason: Test hitted a banned word _kw
def test_verify_kwargs_failure(self):
    codestr = """
        def x(a: int=1, b: str="hunter2") -> None:
            return
        x(a="hi", b="lol")
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, r"Exact\[str\] received for keyword arg 'a', expected int"
    ):
        self.compile(codestr)
