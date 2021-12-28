# Reason: Test hitted a banned word test_default_type_error
def test_default_type_error(self):
    codestr = """
    def foo(x: int = "") -> int:
        return x
    """
    self.type_error(
        codestr, r"type mismatch: Exact\[str\] cannot be assigned to int"
    )
