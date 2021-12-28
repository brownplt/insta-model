# Reason: Test hitted a banned word test_compile_checked_dict_from_dict_call
def test_compile_checked_dict_from_dict_call(self):
    codestr = """
        from __static__.compiler_flags import checked_dicts
        def testfunc():
            x = dict(x=42)
            return x
    """
    with self.assertRaisesRegex(
        TypeError, "cannot create '__static__.chkdict\\[K, V\\]' instances"
    ):
        with self.in_module(codestr) as mod:
            test = mod.testfunc
            test()
