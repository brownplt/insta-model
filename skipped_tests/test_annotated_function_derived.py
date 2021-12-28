# Reason: Can't be translated by any of the three translator
def test_annotated_function_derived(self):
    codestr = """
        class C:
            def f(self) -> int:
                return 1
        class D(C):
            def f(self) -> int:
                return 2
        class E(C):
            pass
        def x(c: C,):
            x = c.f()
            x += c.f()
            return x
    """
    code = self.compile(codestr, modname="test_annotated_function_derived")
    x = self.find_code(code, "x")
    self.assertInBytecode(
        x, "INVOKE_METHOD", (("test_annotated_function_derived", "C", "f"), 0)
    )
    with self.in_module(codestr) as mod:
        x = mod.x
        self.assertEqual(x(mod.C()), 2)
        self.assertEqual(x(mod.D()), 4)
        self.assertEqual(x(mod.E()), 2)
