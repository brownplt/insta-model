# Reason: Test hitted some skipped words
def test_load_iterable_arg_star_not_last(self):
    codestr = """
    def x(a: int, b: int, c: str, d: float, e: float) -> int:
        return 7
    def y() -> int:
        p = (1, 3, 'abc', 0.1)
        return x(*p, 1.0)
    """
    y = self.find_code(self.compile(codestr, modname="foo"), name="y")
    # we should fallback to the normal Python compiler for this
    self.assertNotInBytecode(y, "LOAD_ITERABLE_ARG")
    with self.in_module(codestr) as mod:
        y_callable = mod.y
        self.assertEqual(y_callable(), 7)
