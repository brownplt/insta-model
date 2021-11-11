# Reason: Test hitted some skipped words
def test_final_reassigned_in_ctxmgr(self):
    codestr = """
    from typing import Final
    x: Final[int] = 0xdeadbeef
    with open("lol") as x:
        pass
    """
    with self.assertRaisesRegex(
        TypedSyntaxError, "Cannot assign to a Final variable"
    ):
        self.compile(codestr, modname="foo")
