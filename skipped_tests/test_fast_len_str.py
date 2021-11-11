# Reason: Format too complicated
def test_fast_len_str(self):
    codestr = """
    def f():
        l = "my str!"
        return len(l)
    """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    self.assertInBytecode(f, "FAST_LEN", FAST_LEN_STR)
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(f(), 7)
