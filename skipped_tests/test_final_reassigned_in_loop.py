def test_final_reassigned_in_loop(self):
    codestr = """
    from typing import Final
    x: Final[int] = 0xdeadbeef
    for x in [1, 3, 5]:
        pass
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Cannot assign to a Final variable"
    ):
        self.compile(codestr, modname="foo")
