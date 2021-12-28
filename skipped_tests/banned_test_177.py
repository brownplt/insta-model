# Reason: Test hitted a banned word vararg
def test_varargs_call(self):
    codestr = """
        def g(*foo):
            return foo
        def testfunc():
            return g(2)
    """
    with self.in_module(codestr) as mod:
        test = mod.testfunc
        self.assertEqual(test(), (2,))
