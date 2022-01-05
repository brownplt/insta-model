# test_invoke_chkdict_method.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict
def dict_maker() -> CheckedDict[int, int]:
    return CheckedDict[int, int]({2:2})
def func():
    a = dict_maker()
    return a.keys()
assert list(f()) == [2]

# def test_invoke_chkdict_method(self):
#     codestr = """
#     from __static__ import CheckedDict
#     def dict_maker() -> CheckedDict[int, int]:
#         return CheckedDict[int, int]({2:2})
#     def func():
#         a = dict_maker()
#         return a.keys()
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.func
#         self.assertInBytecode(
#             f,
#             "INVOKE_FUNCTION",
#             (
#                 (
#                     "__static__",
#                     "chkdict",
#                     (("builtins", "int"), ("builtins", "int")),
#                     "keys",
#                 ),
#                 1,
#             ),
#         )
#         self.assertEqual(list(f()), [2])
#         self.assert_jitted(f)
