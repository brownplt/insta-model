# Reason: Test hitted a banned word box
def test_ret_void(self):
    codestr = """
        from xxclassloader import spamobj
        from __static__ import box
        def testfunc():
            x = spamobj[str]()
            y = x.setstate('abc')
            return y
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
        self.assertEqual(test(), None)
