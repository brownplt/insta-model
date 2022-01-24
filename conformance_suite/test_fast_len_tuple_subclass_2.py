# test_fast_len_tuple_subclass_2.py
# This should pass.
# This should terminate.

class mytuple(tuple):
    def __len__(self):
        return 1111
def f(x, a, b):
    l = (a, b)
    if x:
        l = mytuple([a, b])
    return len(l)
def main(f):
    assert f(True, 1, 2) == 1111

main(f)
# def test_fast_len_tuple_subclass_2(self):
#     codestr = """
#     class mytuple(tuple):
#         def __len__(self):
#             return 1111
#     def f(x, a, b):
#         l = (a, b)
#         if x:
#             l = mytuple([a, b])
#         return len(l)
#     """
#     c = self.compile(codestr, modname="foo.py")
#     f = self.find_code(c, "f")
#     self.assertInBytecode(f, "FAST_LEN", FAST_LEN_TUPLE | FAST_LEN_INEXACT)
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertEqual(f(True, 1, 2), 1111)
