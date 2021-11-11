# Reason: Test hitted some skipped words
def test_final_generic(self):
    codestr = """
    from typing import Final
    x: Final[int] = 0xdeadbeef
    """
    self.compile(codestr, modname="foo")
