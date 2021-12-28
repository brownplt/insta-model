# Reason: Test hitted a banned word _kw
def test_verify_kwdefaults_too_many_class(self):
    codestr = """
        class C:
            def x(self, *, b: str="hunter2") -> None:
                return
        C().x('abc')
    """
    # We do not verify types for calls that we can't do direct invokes.
    self.compile(codestr)
