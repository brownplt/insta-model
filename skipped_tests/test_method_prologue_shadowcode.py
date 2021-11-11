# Reason: Test hitted some skipped words
def test_method_prologue_shadowcode(self):
    codestr = """
    def f(x, y: str):
        return 42
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(f, "CHECK_ARGS", (1, ("builtins", "str")))
        for i in range(100):
            self.assertEqual(f("abc", "abc"), 42)
        with self.assertRaisesRegex(
            TypeError, ".*expected 'str' for argument y, got 'int'"
        ):
            f("abc", 42)
