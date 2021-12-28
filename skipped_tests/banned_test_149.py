# Reason: Test hitted a banned word _kw
def test_verify_kwdefaults_no_value(self):
    codestr = """
        def x(*, b: str="hunter2"):
            return b
        a = x()
    """
    module = self.compile(codestr)
    # we don't yet support optimized dispatch to kw-only functions
    self.assertInBytecode(module, "CALL_FUNCTION")
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.a, "hunter2")
