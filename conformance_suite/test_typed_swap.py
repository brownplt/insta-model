# test_typed_swap.py
# This should fail.

def test(a):
    x: int
    y: str
    x, y = 1, a
# def test_typed_swap(self):
#     codestr = """
#         def test(a):
#             x: int
#             y: str
#             x, y = 1, a
#     """
#     f = self.find_code(self.compile(codestr, modname="foo"))
#     self.assertInBytecode(f, "CAST", ("builtins", "str"))
#     self.assertNotInBytecode(f, "CAST", ("builtins", "int"))
