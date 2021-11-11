# Reason: Test hitted some skipped words
def test_narrow_while_break_if(self):
    codestr = """
        from typing import Optional
        def f(x: Optional[int]) -> int:
            while True:
                if x is None:
                    break
                return x
    """
    self.compile(codestr)
