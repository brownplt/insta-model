# test_typed_swap_nested.py
# This should pass.
# This should terminate.
# This should be optimized.

def test(a):
    x: int
    y: str
    z: str
    ((x, y), z) = (a, 'abc'), 'foo'
# def test_typed_swap_nested(self):
#     codestr = """
#         def test(a):
#             x: int
#             y: str
#             z: str
#             ((x, y), z) = (a, 'abc'), 'foo'
#     """
#     f = self.find_code(self.compile(codestr, modname="foo"))
#     self.assertInBytecode(f, "CAST", ("builtins", "int"))
#     self.assertNotInBytecode(f, "CAST", ("builtins", "str"))
