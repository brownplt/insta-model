# Reason: Format too complicated
def test_load_iterable_arg_sequence(self):
    codestr = """
    def x(a: int, b: int, c: str, d: float, e: float) -> int:
        return 7
    def y() -> int:
        p = ["hi", 0.1, 0.2]
        return x(1, 3, *p)
    """
    y = self.find_code(self.compile(codestr, modname="foo"), name="y")
    self.assertInBytecode(y, "LOAD_ITERABLE_ARG", 0)
    self.assertInBytecode(y, "LOAD_ITERABLE_ARG", 1)
    self.assertInBytecode(y, "LOAD_ITERABLE_ARG", 2)
    self.assertNotInBytecode(y, "LOAD_ITERABLE_ARG", 3)
    with self.in_module(codestr) as mod:
        y_callable = mod.y
        self.assertEqual(y_callable(), 7)
