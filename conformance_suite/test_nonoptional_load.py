# test_nonoptional_load.py
# This should pass.

class C:
    def __init__(self, y: int):
        self.y = y
def f(c: C) -> int:
    return c.y
# def test_nonoptional_load(self):
#     codestr = """
#         class C:
#             def __init__(self, y: int):
#                 self.y = y
#         def f(c: C) -> int:
#             return c.y
#     """
#     code = self.compile(codestr, modname="foo")
#     f = self.find_code(code, "f")
#     self.assertInBytecode(f, "LOAD_FIELD", ("foo", "C", "y"))
