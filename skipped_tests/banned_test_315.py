# Reason: Test hitted a banned word test_compile_checked_dict_bad_annotation
def test_compile_checked_dict_bad_annotation(self):
    codestr = """
        from __static__ import CheckedDict
        def testfunc():
            x: 42 = CheckedDict[str, str]({'abc':'abc'})
            return x
    """
    with self.in_module(codestr) as mod:
        test = mod.testfunc
        self.assertEqual(type(test()), chkdict[str, str])
