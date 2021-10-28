def test_assign_while_returns_but_assigns_first(self):
    codestr = """
        from typing import Optional
        def f(x: Optional[int]) -> int:
            y: Optional[int] = 1
            while x is None:
                y = None
                return 1
            return y
    """
    self.compile(codestr, modname="foo")
