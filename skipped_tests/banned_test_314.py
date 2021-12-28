# Reason: Test hitted a banned word f"
def test_compile_dict_setitem_subscr(self):
    codestr = """
        from __static__ import CheckedDict
        def testfunc():
            x = CheckedDict[int, str]({1:'abc'})
            x[2] = 'def'
            return x
    """
    with self.in_module(codestr) as mod:
        test = mod.testfunc
        x = test()
        self.assertInBytecode(
            test,
            "INVOKE_FUNCTION",
            (
                (
                    "__static__",
                    "chkdict",
                    (("builtins", "int"), ("builtins", "str")),
                    "__setitem__",
                ),
                3,
            ),
        )
        self.assertEqual(x, {1: "abc", 2: "def"})
