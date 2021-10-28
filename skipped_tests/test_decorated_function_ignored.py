def test_decorated_function_ignored(self):
    codestr = """
        class C: pass
        def mydecorator(x):
            return C
        @mydecorator
        def f():
            return 42
        def g():
            return f()
    """
    with self.in_module(codestr) as mod:
        C = mod.C
        g = mod.g
        self.assertNotInBytecode(g, "INVOKE_FUNCTION")
        self.assertEqual(type(g()), C)
