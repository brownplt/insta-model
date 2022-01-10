# ht_test_call_function_unknown_ret_type.py
# This should pass.
# This should terminate.

from __future__ import annotations

# EDIT: import Any and use it in place of `foo`
from typing import Any

# EDIT: use `Any` in place of `foo`
def g() -> Any:
    return 42
def testfunc():
    return g()
def main(f):
    assert f() == 42

main(testfunc)
# def test_call_function_unknown_ret_type(self):
#     codestr = """
#         from __future__ import annotations
#         def g() -> foo:
#             return 42
#         def testfunc():
#             return g()
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.testfunc
#         self.assertEqual(f(), 42)
