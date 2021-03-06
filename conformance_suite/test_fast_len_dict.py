# test_fast_len_dict.py
# This should pass.
# This should terminate.

def f():
    l = {1: 'a', 2: 'b', 3: 'c', 4: 'd'}
    return len(l)
def main(f):
    assert f() == 4

main(f)
# def test_fast_len_dict(self):
#     codestr = """
#     def f():
#         l = {1: 'a', 2: 'b', 3: 'c', 4: 'd'}
#         return len(l)
#     """
#     c = self.compile(codestr, modname="foo.py")
#     f = self.find_code(c, "f")
#     self.assertInBytecode(f, "FAST_LEN", FAST_LEN_DICT)
#     with self.in_module(codestr) as mod:
#         f = mod.f
#         self.assertEqual(f(), 4)
