# test_ret_type_cast.py
# This should pass.
# This should terminate.

from typing import Any
def testfunc(x: str, y: str) -> bool:
    return x == y
def main(f):
    assert f('abc', 'abc') == True

main(testfunc)
# def test_ret_type_cast(self):
#     codestr = """
#         from typing import Any
#         def testfunc(x: str, y: str) -> bool:
#             return x == y
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.testfunc
#         self.assertEqual(f("abc", "abc"), True)
#         self.assertInBytecode(f, "CAST", ("builtins", "bool"))
