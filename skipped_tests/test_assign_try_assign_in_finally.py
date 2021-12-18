# Reason: Test hitted some skipped words
def test_assign_try_assign_in_finally(self):
    codestr = """
        class B:
            def f(self):
                return 42
        class D(B):
            def f(self):
                return 'abc'
        def testfunc():
            x: B = D()
            try:
                pass
            finally:
                x = B()
            return x.f()
    """
    code = self.compile(codestr, modname="foo")
    f = self.find_code(code, "testfunc")
    self.assertInBytecode(f, "INVOKE_METHOD", (("foo", "B", "f"), 0))
    with self.in_module(codestr) as mod:
        test = mod.testfunc
        self.assertEqual(test(), 42)
