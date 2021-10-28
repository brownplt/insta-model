def test_compile_checked_dict_with_annotation_wrong_value_type(self):
    codestr = """
        from __static__ import CheckedDict
        class B: pass
        def testfunc():
            x: CheckedDict[B, int] = {B():'hi'}
            return x
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        type_mismatch(
            "Exact[chkdict[foo.B, Exact[str]]]",
            "Exact[chkdict[foo.B, int]]",
        ),
    ):
        self.compile(codestr, modname="foo")
