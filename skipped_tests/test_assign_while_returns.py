def test_assign_while_returns(self):
    codestr = """
        from typing import Optional
        def f(x: Optional[int]) -> int:
            while x is None:
                return 1
            return x
    """
    self.compile(codestr, modname="foo")
