# Reason: Test hitted a banned word _kw
def test_verify_mixed_args_kw_failure_method(self):
    codestr = """
        class C:
            def x(self, a: int=1, b: str="hunter2", c: int=14) -> None:
                return
        C().x(12, c=b'lol', b="lol")
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, r"bytes received for keyword arg 'c', expected int"
    ):
        self.compile(codestr)
