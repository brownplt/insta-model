# Reason: Format too complicated
def test_fast_len_conditional_tuple_funcarg(self):
    codestr = """
        def z(b: object) -> bool:
            return bool(b)
        def f(n: int) -> bool:
            l = tuple(i for i in range(n))
            if z(l):
                return True
            return False
        """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    # Since the tuple is given to z(), do not optimize the check
    # with FAST_LEN
    self.assertNotInBytecode(f, "FAST_LEN")
    with self.in_module(codestr) as mod:
        f = mod.f
        for length in [0, 7]:
            self.assertEqual(f(length), length > 0)
