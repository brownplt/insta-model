def test_default_type_error_with_keywords(self):
    codestr = """
    def foo(x: int, *, y: int, z: int = "") -> int:
        return x + y + z
    """
    self.type_error(
        codestr, r"type mismatch: Exact\[str\] cannot be assigned to int"
    )
