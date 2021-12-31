# test_unknown_type_compare.py
# This should pass.
# This is an optimization test.
# This should terminate.

def x(a, b):
    z = a > b
# def test_unknown_type_compare(self):
#     codestr = """
#         def x(a, b):
#             z = a > b
#     """
#     f = self.find_code(self.compile(codestr, modname="foo"))
#     self.assertInBytecode(f, "COMPARE_OP")
