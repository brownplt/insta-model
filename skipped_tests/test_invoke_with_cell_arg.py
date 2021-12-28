# Reason: Code hitted some skipped words
def test_invoke_with_cell_arg(self):
    codestr = """
        def f(l: list, x: int):
            return [x + y for y in l]
        def g():
            return f([1,2,3], 2)
    """
    with self.in_strict_module(codestr) as mod:
        g = mod.g
        self.assertEqual(g(), [3, 4, 5])
        self.assertInBytecode(g, "INVOKE_FUNCTION", ((mod.__name__, "f"), 2))
