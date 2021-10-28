def test_nested_fn_type_error(self):
    codestr = """
    def f(i: int, j: str, l: int, m: int, n: int, o: int) -> bool:
        def g(k: int) -> bool:
            return k > 0 if j == "gt" else k <= 0
        return g(i)
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        with self.assertRaisesRegex(
            TypeError, r"f expected 'int' for argument n, got 'str'"
        ):
            f(1, "a", 2, 3, "4", 5)
