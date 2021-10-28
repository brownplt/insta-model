def test_final_generic_reassign(self):
    codestr = """
    from typing import Final
    x: Final[int] = 0xdeadbeef
    x = 0x5ca1ab1e
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Cannot assign to a Final variable"
    ):
        self.compile(codestr, modname="foo")
