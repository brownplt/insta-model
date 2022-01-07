# test_aug_assign.py
# This should pass.
# This should terminate.

def f(l):
    l[0] += 1
def main(f):
    l = [1]
    f(l)
    assert l[0] == 2

main(f)
# def test_aug_assign(self) -> None:
#     codestr = """
#     def f(l):
#         l[0] += 1
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         l = [1]
#         f(l)
#         self.assertEqual(l[0], 2)
