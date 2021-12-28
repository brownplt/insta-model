# Reason: Can't be translated by any of the three translator
def test_fast_len_str_subclass_2(self):
    codestr = """
    class mystr(str):
        def __len__(self):
            return 1111
    def f(x):
        s = "abc"
        if x:
            s = mystr("pqr")
        return len(s)
    """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    self.assertInBytecode(f, "FAST_LEN", FAST_LEN_STR | FAST_LEN_INEXACT)
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(f(True), 1111)
        self.assertEqual(f(False), 3)
