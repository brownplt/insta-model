# Reason: Test hitted a banned word int64
def test_compile_checked_dict_create_with_dictcomp(self):
    codestr = """
        from __static__ import CheckedDict, clen, int64
        def testfunc() -> None:
            x = CheckedDict[int, str]({int(i): int(i) for i in
                           range(1, 5)})
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        type_mismatch(
            "Exact[chkdict[Exact[int], Exact[int]]]", "Exact[chkdict[int, str]]"
        ),
    ):
        self.compile(codestr)
