# test_multiply_list_exact_by_int_reverse.py
# This should pass.
# This is an optimization test.
# This should terminate.

def f() -> int:
    l = 2 * [1, 2, 3]
    return len(l)
# def test_multiply_list_exact_by_int_reverse(self):
#     codestr = """
#         def f() -> int:
#             l = 2 * [1, 2, 3]
#             return len(l)
#     """
#     f = self.find_code(self.compile(codestr))
#     self.assertInBytecode(f, "FAST_LEN", FAST_LEN_LIST)
#     with self.in_module(codestr) as mod:
#         self.assertEqual(mod.f(), 6)
