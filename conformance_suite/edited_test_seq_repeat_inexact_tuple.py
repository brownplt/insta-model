# test_seq_repeat_inexact_tuple.py
# This should pass.
# This should terminate.

from typing import Tuple
def f(t: Tuple[int]):
    return t * 2
# The next three line are moved here from the body of `main`
class MyTuple(tuple):
    def __mul__(self, other):
        return 'RESULT'
def main():
    assert f((1, 2)) == (1, 2, 1, 2)
    # class MyTuple(tuple):
    #     def __mul__(self, other):
    #         return 'RESULT'
    assert f(MyTuple((1, 2))) == 'RESULT'

main()
# def test_seq_repeat_inexact_tuple(self):
#     codestr = """
#         from typing import Tuple
#         def f(t: Tuple[int]):
#             return t * 2
#     """
#     f = self.find_code(self.compile(codestr))
#     self.assertInBytecode(f, "SEQUENCE_REPEAT", SEQ_TUPLE | SEQ_REPEAT_INEXACT_SEQ)
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f((1, 2)), (1, 2, 1, 2))
#         class MyTuple(tuple):
#             def __mul__(self, other):
#                 return "RESULT"
#         self.assertEqual(mod.f(MyTuple((1, 2))), "RESULT")
