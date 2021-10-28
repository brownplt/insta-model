# test_assign_subtype_handling.py
# This should pass.

class B: pass
class D(B): pass
def f():
    b: B = B()
    b = D()
    b = B()
# def test_assign_subtype_handling(self):
#     codestr = """
#         class B: pass
#         class D(B): pass
#         def f():
#             b: B = B()
#             b = D()
#             b = B()
#     """
#     self.compile(codestr, modname="foo")
