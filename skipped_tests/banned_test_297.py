# Reason: Test hitted a banned word box
def test_generic_type(self):
    codestr = """
        from xxclassloader import spamobj
        from __static__ import box
        def testfunc():
            x = spamobj[str]()
            x.setstate('abc')
            x.setint(42)
            return (x.getstate(), box(x.getint()))
    """
    code = self.compile(codestr, modname="foo")
    f = self.find_code(code, "testfunc")
    self.assertInBytecode(
        f,
        "INVOKE_METHOD",
        ((("xxclassloader", "spamobj", (("builtins", "str"),), "setstate"), 1)),
    )
    with self.in_module(codestr) as mod:
        test = mod.testfunc
        self.assertEqual(test(), ("abc", 42))
