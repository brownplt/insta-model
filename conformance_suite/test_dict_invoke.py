# test_dict_invoke.py
# This should pass.
# This is an optimization test.
# This should terminate.

from __static__ import pydict
def f(x):
    y: pydict = x
    return y.get('foo')
# def test_dict_invoke(self):
#     codestr = """
#         from __static__ import pydict
#         def f(x):
#             y: pydict = x
#             return y.get('foo')
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(f, "INVOKE_METHOD", (("builtins", "dict", "get"), 1))
#         self.assertEqual(f({}), None)
