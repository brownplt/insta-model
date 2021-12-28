# Reason: Test hitted a banned word _kw
def test_verify_kwonly_failure(self):
    codestr = """
        def x(*, a: int=1, b: str="hunter2") -> None:
            return
        x(a="hi", b="lol")
    """
    # We do not verify types for calls that we can't do direct invokes.
    self.compile(codestr)
