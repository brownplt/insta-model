# Reason: Test hitted some skipped words
def test_donotcompile_method(self):
    codestr = """
    from __static__ import _donotcompile
    def a() -> int:
        return 1
    class C:
        @_donotcompile
        def fn() -> None:
            a() + 2
        def fn2() -> None:
            a() + 2
    c = C()
    """
    with self.in_module(codestr) as mod:
        C = mod.C
        fn2 = C.fn2
        self.assertNotInBytecode(fn2, "CALL_FUNCTION")
        self.assertInBytecode(fn2, "INVOKE_FUNCTION")
        self.assertTrue(fn2.__code__.co_flags & CO_STATICALLY_COMPILED)
        self.assertEqual(fn2(), None)
