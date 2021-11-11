# Reason: Format too complicated
def test_strict_module_constant(self) -> None:
    code = """
        def f(a):
            x: bool = a
    """
    acomp = self.compile_strict(code)
    x = self.find_code(acomp, "f")
    self.assertInBytecode(x, "CAST", ("builtins", "bool"))
