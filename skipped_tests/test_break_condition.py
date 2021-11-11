# Reason: Test hitted some skipped words
def test_break_condition(self):
    codestr = """
        from typing import Optional
        def f(x: Optional[str]) -> str:
            while True:
                if x is None:
                    break
                return x
    """
    self.compile(codestr, modname="foo")
