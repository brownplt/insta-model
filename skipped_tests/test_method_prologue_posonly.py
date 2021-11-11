# Reason: Test hitted some skipped words
def test_method_prologue_posonly(self):
    codestr = """
    def f(x: int, /, y: str):
        return 42
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertInBytecode(
            f, "CHECK_ARGS", (0, ("builtins", "int"), 1, ("builtins", "str"))
        )
        with self.assertRaisesRegex(
            TypeError, ".*expected 'str' for argument y, got 'int'"
        ):
            f(42, 42)
