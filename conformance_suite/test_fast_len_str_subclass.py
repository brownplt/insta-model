# test_fast_len_str_subclass.py
# This should pass.
# This should terminate.

class mystr(str):
    def __len__(self):
        return 1111
def f():
    s = mystr("a")
    return len(s)
def main(f):
    assert f() == 1111

main(f)
# def test_fast_len_str_subclass(self):
#     codestr = """
#     class mystr(str):
#         def __len__(self):
#             return 1111
#     def f():
#         s = mystr("a")
#         return len(s)
#     """
#     c = self.compile(codestr, modname="foo.py")
#     f = self.find_code(c, "f")
#     self.assertNotInBytecode(f, "FAST_LEN")
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertEqual(f(), 1111)
