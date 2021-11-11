# Reason: Format too complicated
def test_invoke_new_derived_nonfunc(self):
    codestr = """
        class C:
            def f(self):
                return 1
        def x(c: C):
            x = c.f()
            x += c.f()
            return x
    """
    code = self.compile(codestr, modname="foo")
    x = self.find_code(code, "x")
    self.assertInBytecode(x, "INVOKE_METHOD", (("foo", "C", "f"), 0))
    with self.in_module(codestr) as mod:
        x, C = mod.x, mod.C
        self.assertEqual(x(C()), 2)
        class Callable:
            def __call__(self_, obj):
                self.assertTrue(isinstance(obj, D))
                return 42
        class D(C):
            f = Callable()
        d = D()
        self.assertEqual(x(d), 84)
