# test_fast_len_list.py
# This should pass.
# This should terminate.

def f():
    l = [1, 2, 3, 4, 5, 6, 7]
    return len(l)
def main(f):
    assert f() == 7

main(f)
# def test_fast_len_list(self):
#     codestr = """
#     def f():
#         l = [1, 2, 3, 4, 5, 6, 7]
#         return len(l)
#     """
#     c = self.compile(codestr, modname="foo.py")
#     f = self.find_code(c, "f")
#     self.assertInBytecode(f, "FAST_LEN", FAST_LEN_LIST)
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertEqual(f(), 7)
