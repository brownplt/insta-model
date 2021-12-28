# Reason: Can't be translated by any of the three translator
def test_invoke_new_derived(self):
    codestr = """
        class C:
            def f(self):
                return 1
        def x(c: C):
            x = c.f()
            x += c.f()
            return x
        a = x(C())
        class D(C):
            def f(self):
                return 2
        b = x(D())
    """
    code = self.compile(codestr, modname="foo")
    x = self.find_code(code, "x")
    self.assertInBytecode(x, "INVOKE_METHOD", (("foo", "C", "f"), 0))
    with self.in_module(codestr) as mod:
        a, b = mod.a, mod.b
        self.assertEqual(a, 2)
        self.assertEqual(b, 4)
