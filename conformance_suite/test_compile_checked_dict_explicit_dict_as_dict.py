# test_compile_checked_dict_explicit_dict_as_dict.py
# This should pass.
# This should terminate.


from __static__ import pydict as dict
class B: pass
class D(B): pass
def testfunc():
    x: dict = {B():42, D():42}
    return x
# def test_compile_checked_dict_explicit_dict_as_dict(self):
#     codestr = """
#         from __static__ import pydict as dict
#         class B: pass
#         class D(B): pass
#         def testfunc():
#             x: dict = {B():42, D():42}
#             return x
#     """
#     with self.in_module(codestr) as mod:
#         test = mod.testfunc
#         self.assertEqual(type(test()), dict)
