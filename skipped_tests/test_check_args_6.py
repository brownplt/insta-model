def test_check_args_6(self):
    """
    Tests whether CHECK_ARGS can handle variables which are in a Cell,
    and are a pos-only arg.
    """
    codestr = """
        def use(i: object) -> object:
            return i
        def outer(x: int, /, y: str) -> object:
            def inner() -> None:
                use(y)
            return use(y)
    """
    with self.in_module(codestr) as mod:
        outer = mod.outer
        self.assertEqual(outer(1, "hi"), "hi")
