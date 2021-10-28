def test_fast_len_set_subclass_2(self):
    codestr = """
    class myset(set):
        def __len__(self):
            return 1111
    def f(x, a, b):
        l = {a, b}
        if x:
            l = myset([a, b])
        return len(l)
    """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    self.assertInBytecode(f, "FAST_LEN", FAST_LEN_SET | FAST_LEN_INEXACT)
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(f(True, 1, 2), 1111)
