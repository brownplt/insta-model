def test_check_args(self):
    """
    Tests whether CHECK_ARGS can handle variables which are in a Cell,
    and are a positional arg at index 0.
    """
    codestr = """
        def use(i: object) -> object:
            return i
        def outer(x: int) -> object:
            def inner() -> None:
                use(x)
            return use(x)
    """
    with self.in_module(codestr) as mod:
        outer = mod.outer
        self.assertEqual(outer(1), 1)
