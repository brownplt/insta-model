# Reason: Test hitted some skipped words
def test_final_generic_types(self):
    codestr = """
    from typing import Final
    def g(i: int) -> int:
        return i
    def f() -> int:
        x: Final[int] = 0xdeadbeef
        return g(x)
    """
    self.compile(codestr, modname="foo")
