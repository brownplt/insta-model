# Reason: Format too complicated
def test_fast_len_set_subclass(self):
    codestr = """
    class myset(set):
        def __len__(self):
            return 1111
    def f():
        l = myset([1, 2])
        return len(l)
    """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    self.assertNotInBytecode(f, "FAST_LEN")
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(f(), 1111)
