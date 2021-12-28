# Reason: Test hitted a banned word _kw
def test_invoke_str_method_kwarg(self):
    codestr = """
    def func():
        a = 'a b c'
        return a.split(sep='a')
    """
    with self.in_module(codestr) as mod:
        f = mod.func
        self.assertNotInBytecode(f, "INVOKE_FUNCTION")
        self.assertNotInBytecode(f, "INVOKE_METHOD")
        self.assertEqual(f(), ["", " b c"])
