def test_fast_len_conditional_str(self):
    codestr = """
        def f(n: int) -> bool:
            l = f"{'a' * n}"
            if l:
                return True
            return False
        """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    self.assertInBytecode(f, "FAST_LEN", FAST_LEN_STR)
    with self.in_module(codestr) as mod:
        f = mod.f
        for length in [0, 7]:
            self.assertEqual(f(length), length > 0)
