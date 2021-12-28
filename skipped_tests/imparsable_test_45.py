# Reason: Format too complicated
def test_check_args_3(self):
    """
    Tests whether CHECK_ARGS can handle variables which are in a Cell,
    and are a positional arg at index > 0.
    """
    codestr = """
        def use(i: object) -> object:
            return i
        def outer(x: int, y: str) -> object:
            def inner() -> None:
                use(y)
            use(x)
            return use(y)
    """
    with self.in_module(codestr) as mod:
        outer = mod.outer
        self.assertEqual(outer(1, "yo"), "yo")
        # Force JIT-compiled code to go through argument checks after
        # keyword arg binding
        self.assertEqual(outer(1, y="yo"), "yo")
