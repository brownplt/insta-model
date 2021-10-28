def test_invoke_frozen_type(self):
    codestr = """
        class C:
            @staticmethod
            def f():
                return 42
        def g():
            return C.f()
    """
    with self.in_module(codestr, freeze=True) as mod:
        g = mod.g
        for i in range(100):
            self.assertEqual(g(), 42)
