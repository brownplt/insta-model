# Reason: Can't be translated by any of the three translator
def test_assert_narrowing_type_error(self):
    codestr = """
    def foo(x: int | str) -> str:
        assert isinstance(x, int)
        return x
    """
    self.type_error(codestr, bad_ret_type("int", "str"))
