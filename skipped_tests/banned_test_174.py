# Reason: Test hitted a banned word _kw
def test_verify_kwdefaults_with_value(self):
    codestr = """
        def x(*, b: str="hunter2"):
            return b
        a = x(b="hunter3")
    """
    module = self.compile(codestr)
    # TODO(T87420170): Support invokes here.
    self.assertNotInBytecode(module, "INVOKE_FUNCTION")
    with self.in_module(codestr) as mod:
        self.assertEqual(mod.a, "hunter3")
