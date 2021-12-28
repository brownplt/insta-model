# Reason: Test hitted a banned word _kw
def test_kwargs_call(self):
    codestr = """
        def g(**foo):
            return foo
        def testfunc():
            return g(x=2)
    """
    with self.in_module(codestr) as mod:
        test = mod.testfunc
        self.assertEqual(test(), {"x": 2})
