def test_invoke_explicit_slots(self):
    codestr = """
        class C:
            __slots__ = ()
            def f(self):
                return 1
        def x(c: C):
            x = c.f()
            x += c.f()
            return x
        a = x(C())
    """
    code = self.compile(codestr, modname="foo")
    x = self.find_code(code, "x")
    self.assertInBytecode(x, "INVOKE_METHOD", (("foo", "C", "f"), 0))
    with self.in_module(codestr) as mod:
        a = mod.a
        self.assertEqual(a, 2)
