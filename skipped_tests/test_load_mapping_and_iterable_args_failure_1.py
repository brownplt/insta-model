# Reason: Test hitted some skipped words
def test_load_mapping_and_iterable_args_failure_1(self):
    """
    Fails because we don't supply enough positional args
    """
    codestr = """
    def x(a: int, b: int, c: str, d: float=2.2, e: float=1.1, f: str="something") -> bool:
        return bool(a == 1 and b == 3 and f == "yo" and d == 2.2 and e == 1.1)
    def y() -> bool:
        return x(1, 3, f="yo")
    """
    with self.assertRaisesRegex(
        SyntaxError, "Function foo.x expects a value for argument c"
    ):
        self.compile(codestr, modname="foo")
