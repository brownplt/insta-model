def test_compile_checked_dict_from_dict_call_3(self):
    # we emit the chkdict import first before future annotations, but that
    # should be fine as we're the compiler.
    codestr = """
        from __future__ import annotations
        from __static__.compiler_flags import checked_dicts
        def testfunc():
            x = dict[str, int](x=42)
            return x
    """
    with self.in_module(codestr) as mod:
        test = mod.testfunc
        self.assertEqual(type(test()), chkdict[str, int])
