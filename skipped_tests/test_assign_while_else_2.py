# Reason: Format too complicated
def test_assign_while_else_2(self):
    codestr = """
        class B:
            def f(self):
                return 42
        class D(B):
            def f(self):
                return 'abc'
        def testfunc(abc):
            x: B = D()
            while abc:
                pass
            else:
                x = B()
            return x.f()
    """
    code = self.compile(codestr, modname="foo")
    f = self.find_code(code, "testfunc")
    self.assertInBytecode(f, "INVOKE_METHOD", (("foo", "B", "f"), 0))
    with self.in_module(codestr) as mod:
        test = mod.testfunc
        self.assertEqual(test(False), 42)
