# Reason: Test hitted a banned word vararg
def test_varargs_count(self):
    codestr = """
        def test(*foo):
            print(foo.count('bar'))
    """
    with self.in_module(codestr) as mod:
        test = mod.test
        self.assertInBytecode(
            test, "INVOKE_FUNCTION", (("builtins", "tuple", "count"), 2)
        )
