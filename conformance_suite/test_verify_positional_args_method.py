# test_verify_positional_args_method.py
# This should pass.

class C:
    def x(self, a: int, b: str) -> None:
        pass
C().x(2, "hi")
# def test_verify_positional_args_method(self):
#     codestr = """
#         class C:
#             def x(self, a: int, b: str) -> None:
#                 pass
#         C().x(2, "hi")
#     """
#     self.compile(codestr)
