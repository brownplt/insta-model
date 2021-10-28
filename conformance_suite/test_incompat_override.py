# test_incompat_override.py
# This should fail.

class C:
    x: int
class D(C):
    def x(self): pass
# def test_incompat_override(self):
#     codestr = """
#     class C:
#         x: int
#     class D(C):
#         def x(self): pass
#     """
#     with self.assertRaises(TypedSyntaxError):
#         self.compile(codestr, modname="foo")
