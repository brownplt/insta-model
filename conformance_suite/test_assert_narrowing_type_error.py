# test_assert_narrowing_type_error.py
# This should fail.

def foo(x: int | str) -> str:
    assert isinstance(x, int)
    return x
# def test_assert_narrowing_type_error(self):
#     codestr = """
#     def foo(x: int | str) -> str:
#         assert isinstance(x, int)
#         return x
#     """
#     self.type_error(codestr, bad_ret_type("int", "str"))
