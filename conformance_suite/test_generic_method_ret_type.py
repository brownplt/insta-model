# test_generic_method_ret_type.py
# This should pass.
# This should terminate.

from __static__ import CheckedDict
from typing import Optional
MAP: CheckedDict[str, Optional[str]] = CheckedDict[str, Optional[str]]({'abc': 'foo', 'bar': None})
def f(x: str) -> Optional[str]:
    return MAP.get(x)
assert f('bar') == None

# def test_generic_method_ret_type(self):
#     codestr = """
#         from __static__ import CheckedDict
#         from typing import Optional
#         MAP: CheckedDict[str, Optional[str]] = CheckedDict[str, Optional[str]]({'abc': 'foo', 'bar': None})
#         def f(x: str) -> Optional[str]:
#             return MAP.get(x)
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertInBytecode(
#             f,
#             "INVOKE_FUNCTION",
#             (
#                 (
#                     "__static__",
#                     "chkdict",
#                     (("builtins", "str"), ("builtins", "str", "?")),
#                     "get",
#                 ),
#                 3,
#             ),
#         )
#         self.assertEqual(f("abc"), "foo")
#         self.assertEqual(f("bar"), None)
