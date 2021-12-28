# Reason: Test hitted a banned word f"
def test_invoke_new_derived_nonfunc_descriptor_inst_override(self):
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
            def __call__(self):
                return 42
        class Descr:
            def __get__(self, inst, ctx):
                return Callable()
        class D(C):
            f = Descr()
        d = D()
        self.assertEqual(x(d), 84)
        d.__dict__["f"] = lambda x: 100
        self.assertEqual(x(d), 200)
