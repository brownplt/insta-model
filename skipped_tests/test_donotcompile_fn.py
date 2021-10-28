def test_donotcompile_fn(self):
    codestr = """
    from __static__ import _donotcompile
    def a() -> int:
        return 1
    @_donotcompile
    def fn() -> None:
        a() + 2
    def fn2() -> None:
        a() + 2
    """
    with self.in_module(codestr) as mod:
        fn = mod.fn
        self.assertInBytecode(fn, "CALL_FUNCTION")
        self.assertNotInBytecode(fn, "INVOKE_FUNCTION")
        self.assertFalse(fn.__code__.co_flags & CO_STATICALLY_COMPILED)
        self.assertEqual(fn(), None)
        fn2 = mod.fn2
        self.assertNotInBytecode(fn2, "CALL_FUNCTION")
        self.assertInBytecode(fn2, "INVOKE_FUNCTION")
        self.assertTrue(fn2.__code__.co_flags & CO_STATICALLY_COMPILED)
        self.assertEqual(fn2(), None)
