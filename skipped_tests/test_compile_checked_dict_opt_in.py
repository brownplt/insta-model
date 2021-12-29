# Reason: Hitted a skipped word (__static__.compiler_flags)
def test_compile_checked_dict_opt_in(self):
    codestr = """
        from __static__.compiler_flags import checked_dicts
        class B: pass
        class D(B): pass
        def testfunc():
            x = {B():42, D():42}
            return x
    """
    with self.in_module(codestr) as mod:
        test = mod.testfunc
        B = mod.B
        self.assertEqual(type(test()), chkdict[B, int])
