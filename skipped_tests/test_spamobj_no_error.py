def test_spamobj_no_error(self):
    codestr = """
        from xxclassloader import spamobj
        def testfunc():
            x = spamobj[int]()
            return x.error(0)
    """
    with self.in_module(codestr) as mod:
        f = mod.testfunc
        self.assertEqual(f(), None)
