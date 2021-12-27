# Reason: Can't be translated by any of the three translator
def test_default_type_error_with_positional_only_arguments(self):
    codestr = """
    def foo(x: int = "", /) -> int:
        return x
    """
    self.type_error(
        codestr, r"type mismatch: Exact\[str\] cannot be assigned to int"
    )
