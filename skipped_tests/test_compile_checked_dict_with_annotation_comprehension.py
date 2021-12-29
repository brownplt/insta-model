# Reason: Hitted a skipped word (test_compile_checked_dict_with_annotation_comprehension)
def test_compile_checked_dict_with_annotation_comprehension(self):
    codestr = """
        from __static__ import CheckedDict
        def testfunc():
            x: CheckedDict[int, object] = {int(i): object() for i in range(1, 5)}
            return x
    """
    with self.in_module(codestr) as mod:
        test = mod.testfunc
        self.assertEqual(type(test()), chkdict[int, object])
