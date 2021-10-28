def test_fast_len_conditional_str_subclass(self):
    codestr = """
        class MyStr(str):
            def __len__(self):
                return 1729
        def f(n: int, flag: bool) -> bool:
            x: str = f"{'a' * n}"
            if flag:
                x = MyStr(f"{'a' * n}")
            if x:
                return True
            return False
        """
    c = self.compile(codestr, modname="foo.py")
    f = self.find_code(c, "f")
    self.assertInBytecode(f, "FAST_LEN", FAST_LEN_STR | FAST_LEN_INEXACT)
    with self.in_module(codestr) as mod:
        f = mod.f
        for boolean, length in itertools.product((True, False), [0, 7]):
            self.assertEqual(
                f(length, boolean),
                length > 0 or boolean,
                f"length={length}, flag={boolean}",
            )
