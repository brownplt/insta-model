# Reason: Test hitted a banned word mixed_args
def test_verify_mixed_args_method(self):
    codestr = """
        class C:
            def x(self, a: int=1, b: str="hunter2", c: int=14) -> None:
                return
        C().x(12, c=56, b="lol")
    """
    self.compile(codestr)
