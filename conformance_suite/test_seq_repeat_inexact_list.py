# test_seq_repeat_inexact_list.py
# This should pass.
# This should terminate.

from typing import List
def f(l: List[int]):
    return l * 2
def main():
    assert f([1, 2]) == [1, 2, 1, 2]
    class MyList(list):

    def __mul__(self, other):
        return 'RESULT'
    assert f(MyList([1, 2])) == 'RESULT'

main()
# def test_seq_repeat_inexact_list(self):
#     codestr = """
#         from typing import List
#         def f(l: List[int]):
#             return l * 2
#     """
#     f = self.find_code(self.compile(codestr))
#     self.assertInBytecode(f, "SEQUENCE_REPEAT", SEQ_LIST | SEQ_REPEAT_INEXACT_SEQ)
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f([1, 2]), [1, 2, 1, 2])
#         class MyList(list):
#             def __mul__(self, other):
#                 return "RESULT"
#         self.assertEqual(mod.f(MyList([1, 2])), "RESULT")
