# test_redefine_local_type.py
# This should fail.

class C: pass
class D: pass
def f():
    x: C = C()
    x: D = D()
# def test_redefine_local_type(self) -> None:
#     codestr = """
#         class C: pass
#         class D: pass
#         def f():
#             x: C = C()
#             x: D = D()
#     """
#     with self.assertRaises(TypedSyntaxError):
#         self.compile(codestr, modname="foo")
