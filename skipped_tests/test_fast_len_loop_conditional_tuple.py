def test_fast_len_loop_conditional_tuple(self):
    codestr = """
        def f(n: int) -> bool:
            l = tuple(i for i in range(n))
            while l:
                return True
            return False
        """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    self.assertInBytecode(f, "FAST_LEN", FAST_LEN_TUPLE)
    with self.in_module(codestr) as mod:
        f = mod.f
        for length in [0, 7]:
            self.assertEqual(f(length), length > 0)
