# Reason: Test hitted a banned word continue
def test_continue_condition(self):
    codestr = """
        from typing import Optional
        def f(x: Optional[str]) -> str:
            while True:
                if x is None:
                    continue
                return x
    """
    self.compile(codestr, modname="foo")
