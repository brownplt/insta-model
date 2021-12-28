# test_for_iter_sequence_orelse.py
# This should pass.
# This should terminate.


from typing import List
def f(n: int) -> List:
    acc = []
    l = [i for i in range(n)]
    for i in l:
        acc.append(i + 1)
    else:
        acc.append(999)
    return acc
# def test_for_iter_sequence_orelse(self):
#     codestr = """
#         from typing import List
#         def f(n: int) -> List:
#             acc = []
#             l = [i for i in range(n)]
#             for i in l:
#                 acc.append(i + 1)
#             else:
#                 acc.append(999)
#             return acc
#     """
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertNotInBytecode(f, "FOR_ITER")
#         self.assertEqual(f(4), [i + 1 for i in range(4)] + [999])
