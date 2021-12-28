# Reason: Test hitted a banned word global
def test_global_call_add(self) -> None:
    codestr = """
        X = ord(42)
        def f():
            y = X + 1
    """
    code = self.compile(codestr, modname="foo")
