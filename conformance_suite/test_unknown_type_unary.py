# test_unknown_type_unary.py
# This should pass.
# This should terminate.
# This should be optimized.

def x(y):
    z = -y
# def test_unknown_type_unary(self):
#     codestr = """
#         def x(y):
#             z = -y
#     """
#     f = self.find_code(self.compile(codestr, modname="foo"))
#     self.assertInBytecode(f, "UNARY_NEGATIVE")
