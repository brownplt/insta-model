# Reason: Test hitted a banned word _kw
def test_kwarg_nocast(self):
    codestr = """
        def x(a: int=1, b: str="hunter2", c: int=14) -> None:
            return
        def g():
            x(b='abc')
    """
    code = self.find_code(self.compile(codestr), "g")
    self.assertNotInBytecode(code, "CAST", ("builtins", "str"))
