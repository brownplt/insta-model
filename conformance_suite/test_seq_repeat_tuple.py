# test_seq_repeat_tuple.py
# This should pass.
# This should terminate.

def f():
    t = (1, 2)
    return t * 2
def main():
    assert f() == (1, 2, 1, 2)

main()
# def test_seq_repeat_tuple(self):
#     codestr = """
#         def f():
#             t = (1, 2)
#             return t * 2
#     """
#     f = self.find_code(self.compile(codestr))
#     self.assertInBytecode(f, "SEQUENCE_REPEAT", SEQ_TUPLE)
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f(), (1, 2, 1, 2))
