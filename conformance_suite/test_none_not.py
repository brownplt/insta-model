# test_none_not.py
# This should pass.
# This should terminate.
# This should be optimized.

def t() -> bool:
    x = None
    if not x:
        return True
    else:
        return False
# def test_none_not(self):
#     codestr = """
#     def t() -> bool:
#         x = None
#         if not x:
#             return True
#         else:
#             return False
#     """
#     with self.in_module(codestr) as mod:
#         t = mod.t
#         self.assertInBytecode(t, "POP_JUMP_IF_TRUE")
#         self.assertTrue(t())