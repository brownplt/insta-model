# Reason: Test hitted a banned word test_if_else_optional_return_two_branches
def test_if_else_optional_return_two_branches(self):
    codestr = """
        from typing import Optional
        class C:
            def __init__(self):
                self.field = self
        def f(x: Optional[C]):
            if x is None:
                if a:
                    return 0
                else:
                    return 2
            return x.field
    """
    self.compile(codestr, modname="foo")
