# Reason: Format too complicated
def test_invoke_base_inited(self):
    """when the base class v-table is initialized before a derived
    class we still have a properly initialized v-table for the
    derived type"""
    codestr = """
        class B:
            def f(self):
                return 42
        X = B().f()
        class D(B):
            def g(self):
                return 100
        def g(x: D):
            return x.g()
    """
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.X, 42)
        d = mod.D()
        self.assertEqual(mod.g(d), 100)
