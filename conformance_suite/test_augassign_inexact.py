# test_augassign_inexact.py
# This should pass.
# This should terminate.

def something():
    return 3
def t():
    a: int = something()
    b = 0
    b += a
    return b
def main(t):
    assert t() == 3

main(t)
# def test_augassign_inexact(self):
#     codestr = """
#     def something():
#         return 3
#     def t():
#         a: int = something()
#         b = 0
#         b += a
#         return b
#     """
#     with self.in_module(codestr) as mod:
#         t = mod.t
#         self.assertInBytecode(t, "INPLACE_ADD")
#         self.assertEqual(t(), 3)
