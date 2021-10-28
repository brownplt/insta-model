def test_compile_checked_dict_ann_differs_2(self):
    codestr = """
        from __static__ import CheckedDict
        def testfunc():
            x: int = CheckedDict[str, str]({'abc':'abc'})
            return x
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        type_mismatch("Exact[chkdict[str, str]]", "int"),
    ):
        self.compile(codestr, modname="foo")
