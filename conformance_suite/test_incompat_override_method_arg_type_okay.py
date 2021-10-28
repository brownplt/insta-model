# test_incompat_override_method_arg_type_okay.py
# This should pass.

class A:
    def m(self, x: str) -> int:
        return 42
class B(A):
    def m(self, x: object) -> int:
        return 0
# def test_incompat_override_method_arg_type_okay(self):
#     codestr = """
#         class A:
#             def m(self, x: str) -> int:
#                 return 42
#         class B(A):
#             def m(self, x: object) -> int:
#                 return 0
#     """
#     self.compile(codestr)
