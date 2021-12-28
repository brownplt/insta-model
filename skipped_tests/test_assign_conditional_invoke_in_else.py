# Reason: Can't be translated by any of the three translator
def test_assign_conditional_invoke_in_else(self):
    codestr = """
        class B:
            def f(self):
                return 42
        class D(B):
            def f(self):
                return 'abc'
        def testfunc(abc):
            x = B()
            if abc:
                x = D()
            else:
                return x.f()
    """
    code = self.compile(codestr, modname="foo")
    f = self.find_code(code, "testfunc")
    self.assertInBytecode(f, "INVOKE_METHOD", (("foo", "B", "f"), 0))
    with self.in_module(codestr) as mod:
        test = mod.testfunc
        self.assertEqual(test(True), None)
