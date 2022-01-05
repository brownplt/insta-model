# test_compile_checked_dict_explicit_dict.py
# This should pass.
# This should terminate.

from __static__ import pydict
class B: pass
class D(B): pass
def testfunc():
    x: pydict = {B():42, D():42}
    return x
assert type(test()) == dict

# def test_compile_checked_dict_explicit_dict(self):
#     codestr = """
#         from __static__ import pydict
#         class B: pass
#         class D(B): pass
#         def testfunc():
#             x: pydict = {B():42, D():42}
#             return x
#     """
#     with self.in_module(codestr) as mod:
#         test = mod.testfunc
#         self.assertEqual(type(test()), dict)
