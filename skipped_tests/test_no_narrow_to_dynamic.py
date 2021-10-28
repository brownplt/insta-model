def test_no_narrow_to_dynamic(self):
    codestr = """
        def f():
            return 42
        def g():
            x: int = 100
            x = f()
            return x.bit_length()
    """
    with self.in_module(codestr) as mod:
        g = mod.g
        self.assertInBytecode(g, "CAST", ("builtins", "int"))
        self.assertInBytecode(
            g, "INVOKE_METHOD", (("builtins", "int", "bit_length"), 0)
        )
        self.assertEqual(g(), 6)
