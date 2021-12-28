# Reason: Can't be translated by any of the three translator
def test_fast_len_str_unicode_chars(self):
    codestr = """
    def f():
        l = "\U0001F923"  # ROFL emoji
        return len(l)
    """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    self.assertInBytecode(f, "FAST_LEN", FAST_LEN_STR)
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertEqual(f(), 1)
