# test_typed_swap_list.py
# This should pass.
# This is an optimization test.

def test(a):
    x: int
    y: str
    [x, y] = a, 'abc'
# def test_typed_swap_list(self):
#     codestr = """
#         def test(a):
#             x: int
#             y: str
#             [x, y] = a, 'abc'
#     """
#     f = self.find_code(self.compile(codestr, modname="foo"))
#     self.assertInBytecode(f, "CAST", ("builtins", "int"))
#     self.assertNotInBytecode(f, "CAST", ("builtins", "str"))
