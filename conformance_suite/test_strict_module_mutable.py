# test_strict_module_mutable.py
# This should pass.
# This should terminate.
# This should be optimized.

from __strict__ import mutable
@mutable
class C:
    def __init__(self, x):
        self.x = 1
# def test_strict_module_mutable(self):
#     code = """
#         from __strict__ import mutable
#         @mutable
#         class C:
#             def __init__(self, x):
#                 self.x = 1
#     """
#     with self.in_module(code) as mod:
#         self.assertInBytecode(mod.C.__init__, "STORE_FIELD")
