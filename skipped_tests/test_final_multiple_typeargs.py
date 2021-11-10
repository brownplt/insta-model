def test_final_multiple_typeargs(self):
    codestr = """
    from typing import Final
    from something import hello
    x: Final[int, str] = hello()
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"incorrect number of generic arguments for Final\[T\], expected 1, got 2",
    ):
        self.compile(codestr, modname="foo")
