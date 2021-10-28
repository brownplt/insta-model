def test_invoke_int_method(self):
    codestr = """
    def func():
        a = 42
        return a.bit_length()
    """
    with self.in_module(codestr) as mod:
        f = mod.func
        self.assertInBytecode(
            f, "INVOKE_FUNCTION", (("builtins", "int", "bit_length"), 1)
        )
        self.assertEqual(f(), 6)
