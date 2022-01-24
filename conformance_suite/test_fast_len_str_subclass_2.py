# test_fast_len_str_subclass_2.py
# This should pass.
# This should terminate.

class mystr(str):
    def __len__(self):
        return 1111
def f(x):
    s = "abc"
    if x:
        s = mystr("pqr")
    return len(s)
def main(f):
    assert f(True) == 1111
    assert f(False) == 3

main(f)
# def test_fast_len_str_subclass_2(self):
#     codestr = """
#     class mystr(str):
#         def __len__(self):
#             return 1111
#     def f(x):
#         s = "abc"
#         if x:
#             s = mystr("pqr")
#         return len(s)
#     """
#     c = self.compile(codestr, modname="foo.py")
#     f = self.find_code(c, "f")
#     self.assertInBytecode(f, "FAST_LEN", FAST_LEN_STR | FAST_LEN_INEXACT)
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertEqual(f(True), 1111)
#         self.assertEqual(f(False), 3)
