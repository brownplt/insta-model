def test_slotification_decorated(self):
    codestr = """
        class _Inner():
            pass
        def something(klass):
            return _Inner
        @something
        class C:
            def f(self):
                pass
        def f():
            return C().f()
    """
    with self.in_module(codestr) as mod:
        f = mod.f
        self.assertNotInBytecode(f, "INVOKE_FUNCTION")
        self.assertNotInBytecode(f, "INVOKE_METHOD")
