def test_final_reassign(self):
    codestr = """
    from typing import Any, Final
    x: Final[Any] = 0xdeadbeef
    x = "something"
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Cannot assign to a Final variable"
    ):
        self.compile(codestr, modname="foo")
