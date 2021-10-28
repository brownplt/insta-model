def test_invoke_with_cell(self):
    codestr = """
        def f(l: list):
            x = 2
            return [x + y for y in l]
        def g():
            return f([1,2,3])
    """
    with self.in_strict_module(codestr) as mod:
        g = mod.g
        self.assertEqual(g(), [3, 4, 5])
        self.assertInBytecode(g, "INVOKE_FUNCTION", ((mod.__name__, "f"), 1))
