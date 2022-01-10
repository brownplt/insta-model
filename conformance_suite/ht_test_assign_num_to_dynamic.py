# test_assign_num_to_dynamic.py
# This should pass.

# We added the next line.
from typing import Any

def f():
    # We edited the next line to use `Any` in place of `foo`
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
