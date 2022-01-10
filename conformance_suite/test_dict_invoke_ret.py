# test_dict_invoke_ret.py
# This should pass.
# This should terminate.

from __static__ import pydict
def g(): return None
def f(x):
    y: pydict = x
    z = y.get('foo')
    z = None  # should be typed to dynamic
    return z
def main(f):
    assert f({}) == None

main(f)
# def test_dict_invoke_ret(self):
#     codestr = """
#         from __static__ import pydict
#         def g(): return None
#         def f(x):
#             y: pydict = x
#             z = y.get('foo')
#             z = None  # should be typed to dynamic
#             return z
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "INVOKE_METHOD", (("builtins", "dict", "get"), 1))
#         self.assertEqual(f({}), None)
