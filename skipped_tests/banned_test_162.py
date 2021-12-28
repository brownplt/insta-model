# Reason: Test hitted a banned word _kw
def test_kwarg_cast(self):
    codestr = """
        def x(a: int=1, b: str="hunter2", c: int=14) -> None:
            return
        def g(a):
            x(b=a)
    """
    code = self.find_code(self.compile(codestr), "g")
    self.assertInBytecode(code, "CAST", ("builtins", "str"))
