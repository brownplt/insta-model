def test_load_iterable_arg_default_overridden(self):
    codestr = """
        def x(a: int, b: int, c: str, d: float = 10.1, e: float = 20.1) -> bool:
            return bool(
                a == 1
                and b == 3
                and c == "hi"
                and d == 0.1
                and e == 0.2
            )
        def y() -> bool:
            p = ("hi", 0.1, 0.2)
            return x(1, 3, *p)
    """
    y = self.find_code(self.compile(codestr, modname="foo"), name="y")
    self.assertNotInBytecode(y, "LOAD_ITERABLE_ARG", 3)
    self.assertNotInBytecode(y, "LOAD_MAPPING_ARG", 3)
    with self.in_module(codestr) as mod:
        y_callable = mod.y
        self.assertTrue(y_callable())
