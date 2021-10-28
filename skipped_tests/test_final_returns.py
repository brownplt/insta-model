def test_final_returns(self):
    codestr = """
    from typing import Final
    def f() -> Final[int]:
        return 1
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        "Final annotation is only valid in initial declaration",
    ):
        self.compile(codestr, modname="foo")
