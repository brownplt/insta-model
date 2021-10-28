def test_final_reassigned_in_loop_target_tuple(self):
    codestr = """
    from typing import Final
    x: Final[int] = 0xdeadbeef
    for x, y in [(1, 2)]:
        pass
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Cannot assign to a Final variable"
    ):
        self.compile(codestr, modname="foo")
