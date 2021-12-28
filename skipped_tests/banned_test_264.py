# Reason: Test hitted a banned word test_compile_checked_dict_wrong_unknown_type
def test_compile_checked_dict_wrong_unknown_type(self):
    codestr = """
        def f(x: int):
            return x
        def testfunc(iter):
            return f({x:42 for x in iter})
    """
    with self.assertRaisesRegex(
        TypedSyntaxError,
        r"Exact\[dict\] received for positional arg 'x', expected int",
    ):
        self.compile(codestr, modname="foo")
