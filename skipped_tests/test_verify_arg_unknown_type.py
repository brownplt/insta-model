def test_verify_arg_unknown_type(self):
    codestr = """
        def x(x:foo):
            return b
        x('abc')
    """
    module = self.compile(codestr)
    self.assertInBytecode(module, "INVOKE_FUNCTION")
    x = self.find_code(module)
    self.assertInBytecode(x, "CHECK_ARGS", ())
