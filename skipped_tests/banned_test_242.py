# Reason: Test hitted a banned word continue
def test_narrow_while_continue_if(self):
    codestr = """
        from typing import Optional
        def f(x: Optional[int]) -> int:
            while True:
                if x is None:
                    continue
                return x
    """
    self.compile(codestr)
