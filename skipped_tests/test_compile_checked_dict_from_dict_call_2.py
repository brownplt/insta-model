def test_compile_checked_dict_from_dict_call_2(self):
    codestr = """
        from __static__.compiler_flags import checked_dicts
        def testfunc():
            x = dict[str, int](x=42)
            return x
    """
    with self.in_module(codestr) as mod:
        test = mod.testfunc
        self.assertEqual(type(test()), chkdict[str, int])
