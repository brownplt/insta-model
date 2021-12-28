# Reason: Test hitted a banned word _kw
def test_verify_kwdefaults_too_many(self):
    codestr = """
        def x(*, b: str="hunter2") -> None:
            return
        x('abc')
    """
    # We do not verify types for calls that we can't do direct invokes.
    self.compile(codestr)
