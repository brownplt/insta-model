# Reason: Test hitted a banned word f"
def test_strict_module(self) -> None:
    code = """
        def f(a):
            x: bool = a
    """
    acomp = self.compile_strict(code)
    x = self.find_code(acomp, "f")
    self.assertInBytecode(x, "CAST", ("builtins", "bool"))
