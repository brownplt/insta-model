# Reason: Test hitted a banned word _kw
def test_verify_kwargs(self):
    codestr = """
        def x(a: int=1, b: str="hunter2") -> None:
            return
        x(b="lol", a=23)
    """
    self.compile(codestr)
