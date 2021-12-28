# Reason: Format too complicated
def test_check_args_7(self):
    """
    Tests whether CHECK_ARGS can handle multiple variables which are in a Cell,
    and are a mix of positional, pos-only and kw-only args.
    """
    codestr = """
        def use(i: object) -> object:
            return i
        def outer(x: int, /, y: int, *, z: str = "lol") -> object:
            def inner() -> None:
                use(x)
                use(y)
                use(z)
            return use(x), use(y), use(z)
    """
    with self.in_module(codestr) as mod:
        outer = mod.outer
        self.assertEqual(outer(3, 2, z="hi"), (3, 2, "hi"))
