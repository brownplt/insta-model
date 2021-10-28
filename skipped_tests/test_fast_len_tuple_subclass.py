def test_fast_len_tuple_subclass(self):
    codestr = """
    class mytuple(tuple):
        def __len__(self):
            return 1111
    def f():
        l = mytuple([1, 2])
        return len(l)
    """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    self.assertNotInBytecode(f, "FAST_LEN")
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(f(), 1111)
