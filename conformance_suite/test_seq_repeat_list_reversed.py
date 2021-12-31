# test_seq_repeat_list_reversed.py
# This should pass.
# This is an optimization test.

def f():
    l = [1, 2]
    return 2 * l
# def test_seq_repeat_list_reversed(self):
#     codestr = """
#         def f():
#             l = [1, 2]
#             return 2 * l
#     """
#     f = self.find_code(self.compile(codestr))
#     self.assertInBytecode(f, "SEQUENCE_REPEAT", SEQ_LIST | SEQ_REPEAT_REVERSED)
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f(), [1, 2, 1, 2])
