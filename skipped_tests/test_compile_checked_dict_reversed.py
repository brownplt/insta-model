def test_compile_checked_dict_reversed(self):
    codestr = """
        from __static__ import CheckedDict
        class B: pass
        class D(B): pass
        def testfunc():
            x = CheckedDict[B, int]({D():42, B():42})
            return x
    """
    with self.in_module(codestr) as mod:
        test = mod.testfunc
        B = mod.B
        self.assertEqual(type(test()), chkdict[B, int])