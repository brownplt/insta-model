# Reason: Test hitted some skipped words
def test_fast_len_conditional_tuple_subclass(self):
    codestr = """
        class Mytuple(tuple):
            def __len__(self):
                return 1729
        def f(n: int, flag: bool) -> bool:
            x = tuple(i for i in range(n))
            if flag:
                x = Mytuple([i for i in range(n)])
            if x:
                return True
            return False
        """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    self.assertInBytecode(f, "FAST_LEN", FAST_LEN_TUPLE | FAST_LEN_INEXACT)
    with self.in_module(codestr) as mod:
        f = mod.f
        for boolean, length in itertools.product((True, False), [0, 7]):
            self.assertEqual(
                f(length, boolean),
                length > 0 or boolean,
                f"length={length}, flag={boolean}",
            )
