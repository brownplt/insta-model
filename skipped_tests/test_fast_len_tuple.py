def test_fast_len_tuple(self):
    codestr = """
    def f(a, b):
        l = (a, b)
        return len(l)
    """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    self.assertInBytecode(f, "FAST_LEN", FAST_LEN_TUPLE)
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(f("a", "b"), 2)
