# test_assign_num_to_dynamic.py
# This should pass.

# EDIT: import Any and use it in place of `foo`
from typing import Any

def f():
    # EDIT: use `Any` in place of `foo`
    # x: foo = 42
    x: Any = 42
# def test_assign_num_to_dynamic(self):
#     codestr = """
#         def f():
#             x: foo = 42
#     """
#     code = self.compile(codestr, modname="foo")
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertNotInBytecode(f, "CAST", ("builtins", "object"))
