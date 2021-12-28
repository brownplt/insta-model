# Reason: Format too complicated
def test_check_args_5(self):
    """
    Tests whether CHECK_ARGS can handle variables which are in a Cell,
    and are a kw-only arg.
    """
    codestr = """
        def use(i: object) -> object:
            return i
        def outer(x: int, *, y: str = "lol") -> object:
            def inner() -> None:
                use(y)
            return use(y)
    """
    with self.in_module(codestr) as mod:
        outer = mod.outer
        self.assertEqual(outer(1, y="hi"), "hi")
