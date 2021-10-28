def test_default_type_error_with_non_defaults(self):
    codestr = """
    def foo(non_default: int, x: int = "") -> int:
        return non_default + x
    """
    self.type_error(
        codestr, r"type mismatch: Exact\[str\] cannot be assigned to int"
    )
