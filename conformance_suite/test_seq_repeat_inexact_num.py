# test_seq_repeat_inexact_num.py
# This should fail.

def f(num: int):
    return num * [1, 2]
# def test_seq_repeat_inexact_num(self):
#     codestr = """
#         def f(num: int):
#             return num * [1, 2]
#     """
#     f = self.find_code(self.compile(codestr))
#     self.assertInBytecode(
#         f,
#         "SEQUENCE_REPEAT",
#         SEQ_LIST | SEQ_REPEAT_INEXACT_NUM | SEQ_REPEAT_REVERSED,
#     )
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f(2), [1, 2, 1, 2])
#         class MyInt(int):
#             def __mul__(self, other):
#                 return "RESULT"
#         self.assertEqual(mod.f(MyInt(2)), "RESULT")
