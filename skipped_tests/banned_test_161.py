# Reason: Test hitted a banned word mixed_args
def test_verify_mixed_args(self):
    codestr = """
        def x(a: int=1, b: str="hunter2", c: int=14) -> None:
            return
        x(12, c=56, b="lol")
    """
    self.compile(codestr)
