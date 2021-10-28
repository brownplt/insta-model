# test_redefine_type.py
# This should fail.

class C: pass
class D: pass
def f(a):
    x: C = C()
    x: D = D()
# def test_redefine_type(self):
#     codestr = """
#         class C: pass
#         class D: pass
#         def f(a):
#             x: C = C()
#             x: D = D()
#     """
#     with self.assertRaises(TypedSyntaxError):
#         self.compile(codestr, modname="foo")
