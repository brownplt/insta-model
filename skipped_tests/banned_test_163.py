# Reason: Test hitted a banned word b"
def test_invoke_str_method(self):
    codestr = """
    def func():
        a = 'a b c'
        return a.split()
    """
    with self.in_module(codestr) as mod:
        f = mod.func
        self.assertInBytecode(
            f, "INVOKE_FUNCTION", (("builtins", "str", "split"), 1)
        )
        self.assertEqual(f(), ["a", "b", "c"])
