# test_try_return_finally.py
# This should pass.
# This should terminate.

from typing import List
def f1(x: List):
    try:
        return
    finally:
        x.append("hi")
def main(f1):
    l = []
    f1(l)
    assert l == ['hi']

main(f1)
# def test_try_return_finally(self):
#     codestr = """
#     from typing import List
#     def f1(x: List):
#         try:
#             return
#         finally:
#             x.append("hi")
#     """
#     with self.in_module(codestr) as mod:
#         f1 = mod.f1
#         l = []
#         f1(l)
#         self.assertEqual(l, ["hi"])
