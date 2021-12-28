# Reason: Format too complicated
def test_fast_len_conditional_dict(self):
    codestr = """
        def f(n: int) -> bool:
            l = {i: i for i in range(n)}
            if l:
                return True
            return False
        """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    self.assertInBytecode(f, "FAST_LEN", FAST_LEN_DICT)
    with self.in_module(codestr) as mod:
        f = mod.f
        for length in [0, 7]:
            self.assertEqual(f(length), length > 0)
