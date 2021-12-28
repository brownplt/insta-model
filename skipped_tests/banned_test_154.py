# Reason: Test hitted a banned word _kw
def test_verify_kwarg_unknown_type(self):
    codestr = """
        def x(x:foo):
            return b
        x(x='abc')
    """
    module = self.compile(codestr)
    self.assertInBytecode(module, "INVOKE_FUNCTION")
    x = self.find_code(module)
    self.assertInBytecode(x, "CHECK_ARGS", ())
