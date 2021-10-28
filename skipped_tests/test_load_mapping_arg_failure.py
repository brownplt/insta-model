def test_load_mapping_arg_failure(self):
    """
    Fails because we supply an extra kwarg
    """
    codestr = """
    def x(a: int, b: int, c: str, d: float=2.2, e: float=1.1, f: str="something") -> bool:
        return bool(a == 1 and b == 3 and f == "yo" and d == 2.2 and e == 1.1)
    def y() -> bool:
        return x(1, 3, "hi", f="yo", g="lol")
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        "Given argument g does not exist in the definition of foo.x",
    ):
        self.compile(codestr, modname="foo")
