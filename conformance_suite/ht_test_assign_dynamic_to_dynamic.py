# test_assign_dynamic_to_dynamic.py
# This should pass.

# EDIT: import Any and use it in place of `unknown`
from typing import Any

def f(C):
    # EDIT: use `Any`` in place of `unknown`
    # x: unknown = C()
    x: Any = C()
# def test_assign_dynamic_to_dynamic(self):
#     codestr = """
#         def f(C):
#             x: unknown = C()
#     """
#     code = self.compile(codestr, modname="foo")
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertNotInBytecode(f, "CAST", ("builtins", "object"))
